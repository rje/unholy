module Main where

import CircuitBreaker (circuitBreaker)
import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket, evaluate, finally)
import Control.Monad (forM, forM_, replicateM_, unless, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C (peekCString)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Graphics.GL
import PulseCourier (pulseCourier)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Info (arch, os)
import System.Mem (performGC)
import Text.Printf (printf)
import Unholy
import qualified Unholy.Animation as Animation
import qualified Unholy.Native as N
import qualified Unholy.Renderer as R
import qualified Unholy.Renderer.GL as GL

main :: IO ()
main = runInBoundThread $ do
  args <- getArgs
  enabled <- getRTSStatsEnabled
  when (not enabled) (ioError (userError "Run with +RTS -T to collect allocations"))
  printf "platform,%s,%s\n" os arch
  putStrLn "CPU timings and allocation exclude startup; warm graphics use the actual renderer."
  when ("--animation" `elem` args) (profileAnimation >> exitSuccess)
  forM_ [1000, 4000] $ \count -> do
    let p = stress count
    evaluate (validateProject p) >>= either (ioError . userError) pure
    measure ("validate-" ++ show count) 400 $ \i -> do
      let w = world p; changed = w {transforms = IM.adjust (\t -> t {position = V3 (fromIntegral i / 1000) 0 0}) 1 (transforms w)}
      value <- evaluate (validateProject p {world = changed})
      either (ioError . userError) pure value
  samples <- forM ["marble-run", "pulse-courier", "circuit-breaker", "material-gallery"] $ \name -> do
    p <- loadProject ("projects/" ++ name ++ "/project.hj.json") >>= either (ioError . userError) pure
    pure (name, p)
  forM_ (take 3 samples) $ \(name, p) -> do
    (runtime, started, _) <- startRuntimeWith [pulseCourier, circuitBreaker] Nothing ("projects/" ++ name) p
    current <- newIORef (runtime, started)
    let tick :: Int -> IO ()
        tick _ = do
          (r, scene) <- readIORef current
          (next, updated, _, _) <- stepGame (1 / 120) (Input 0 0 False False False) r scene
          writeIORef current (next, updated)
    (replicateM_ 240 (tick 0) >> measure ("tick-" ++ name) 240 tick) `finally` stopRuntime runtime
  when ("--gpu" `elem` args) $ N.withWindow "Unholy / measured renderer" "assets" $ \window -> do
    renderer <- N.withRenderer window pure
    writeIORef (R.instancingEnabled renderer) ("--scalar" `notElem` args)
    writeIORef (R.cullingEnabled renderer) ("--unculled" `notElem` args)
    device <- glGetString GL_RENDERER >>= peekCString . castPtr
    putStrLn ("gpu_device," ++ device)
    let district = case samples of (_, marble) : _ -> [("expanded-marble", expandLevel marble)]; _ -> []
    forM_ (samples ++ district ++ [("stress-1000", stress 1000), ("stress-4000", stress 4000)]) $ \(name, p) -> do
      N.unloadAssets window
      let root = if take 7 name == "stress-" then "." else "projects/" ++ if name == "expanded-marble" then "marble-run" else name
          draw = do _ <- N.drawScene window root p (0, 0, 1024, 768) False False Nothing; pure ()
          frame = do _ <- N.frameTime window; N.begin window; draw; N.end window
      replicateM_ 20 frame
      assetErrors <- N.nativeError window
      unless (null assetErrors) (ioError (userError assetErrors))
      viewport <- readIORef (R.viewportTarget renderer)
      stats <- readIORef (R.frameStats renderer)
      putStrLn ("draw_counts," ++ name ++ "," ++ show stats)
      forM_ viewport $ \t -> printf "viewport,%s,%d,%d\n" name (GL.targetWidth t) (GL.targetHeight t)
      performGC
      before <- getRTSStats
      values <- bracket (GL.genObject glGenQueries) (GL.deleteObject glDeleteQueries) $ \query -> forM [1 .. 120 :: Int] $ \_ -> do
        _ <- N.frameTime window
        N.begin window
        glBeginQuery GL_TIME_ELAPSED query
        cpuStart <- getCPUTime
        start <- getMonotonicTimeNSec
        draw
        end <- getMonotonicTimeNSec
        cpuEnd <- getCPUTime
        glEndQuery GL_TIME_ELAPSED
        gpu <- alloca $ \out -> glGetQueryObjectui64v query GL_QUERY_RESULT out >> peek out
        N.end window
        pure (fromIntegral (end - start) / 1e6, fromIntegral (cpuEnd - cpuStart) / 1e9, fromIntegral gpu / 1e6 :: Double)
      performGC
      after <- getRTSStats
      GL.checkGL "GPU timer query profile"
      measuredErrors <- N.nativeError window
      unless (null measuredErrors) (ioError (userError measuredErrors))
      let wall = [w | (w, _, _) <- values]; cpu = [c | (_, c, _) <- values]; gpu = [g | (_, _, g) <- values]
      printf "draw,%s,submission_mean_ms,%.4f,submission_p95_ms,%.4f,cpu_mean_ms,%.4f,gpu_mean_ms,%.4f,gpu_p95_ms,%.4f,frame_alloc_bytes,%.0f\n" name (mean wall) (p95 wall) (mean cpu) (mean gpu) (p95 gpu) (fromIntegral (allocated_bytes after - allocated_bytes before) / 120 :: Double)

measure :: String -> Int -> (Int -> IO ()) -> IO ()
measure label count work = do
  performGC
  before <- getRTSStats
  cpuStart <- getCPUTime
  times <- forM [1 .. count] $ \i -> do start <- getMonotonicTimeNSec; work i; end <- getMonotonicTimeNSec; pure (fromIntegral (end - start) / 1e6 :: Double)
  cpuEnd <- getCPUTime
  performGC
  after <- getRTSStats
  printf "cpu,%s,mean_ms,%.4f,p95_ms,%.4f,cpu_mean_ms,%.4f,alloc_bytes,%.0f\n" label (mean times) (p95 times) (fromIntegral (cpuEnd - cpuStart) / 1e9 / fromIntegral count :: Double) (fromIntegral (allocated_bytes after - allocated_bytes before) / fromIntegral count :: Double)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

p95 :: [Double] -> Double
p95 xs = sort xs !! min (length xs - 1) (length xs * 95 `div` 100)

-- Component comparison, followed by a moving scene through the real renderer.
-- The CPU reference is not an old-renderer frame benchmark.
profileAnimation :: IO ()
profileAnimation = do
  let root = "projects/animation-lab"
  scene <- loadProject (root ++ "/project.hj.json") >>= either (ioError . userError) pure
  actors <- forM (IM.toList (animators (world scene))) $ \(e, config) -> do
    let path = geometry (meshes (world scene) IM.! e)
    rig <- Animation.loadRig (root ++ "/" ++ path)
    pure (rig, config)
  let at frame config = config {animationTime = animationTime config + fromIntegral frame / 120}
      prepare frame =
        VS.sum
          ( VS.fromList
              [ let palette = Animation.posePalette rig (at frame config)
                    matrixSum = VS.foldl' (\total matrix -> total + sum (fmap sum matrix)) 0 palette
                    boundsSum = sum [sum lo + sum hi | mesh <- Animation.rigMeshes rig, let (lo, hi) = Animation.posedBounds rig palette mesh]
                 in matrixSum + boundsSum
              | (rig, config) <- actors
              ]
          )
      reference frame = sum [sum (map VS.sum (Animation.posedMeshes rig (at frame config))) | (rig, config) <- actors]
      moving = filter (animationPlaying . snd) actors
      paletteBytes = sum [VS.length (Animation.posePalette rig config) * 64 | (rig, config) <- moving]
      referenceBytes = sum [sum (map (\mesh -> VS.length (Animation.bindVertices mesh) * 4) (Animation.rigMeshes rig)) | (rig, _) <- moving]
  printf "animation_payload,actors,%d,moving,%d,palette_bytes_per_pose_frame,%d,reference_vertex_bytes_per_pose_frame,%d\n" (length actors) (length moving) paletteBytes referenceBytes
  measure "22-poses-palettes-and-bounds" 120 (\frame -> evaluate (prepare frame) >> pure ())
  measure "22-poses-cpu-vertex-reference" 120 (\frame -> evaluate (reference frame) >> pure ())
  N.withWindow "Unholy / moving Animation Lab profile" "assets" $ \window -> do
    renderer <- N.withRenderer window pure
    device <- glGetString GL_RENDERER >>= peekCString . castPtr
    putStrLn ("gpu_device," ++ device)
    current <- newIORef scene
    let draw = do
          updated <- atomicModifyIORef' current (\p -> let next = p {world = Animation.advanceAnimators (1 / 120) (world p)} in (next, next))
          _ <- N.drawScene window root updated (0, 0, 1024, 768) False False Nothing
          pure ()
        frame = do _ <- N.frameTime window; N.begin window; draw; N.end window
    replicateM_ 20 frame
    viewport <- readIORef (R.viewportTarget renderer)
    forM_ viewport $ \target -> printf "viewport,animation-lab,%d,%d\n" (GL.targetWidth target) (GL.targetHeight target)
    performGC
    before <- getRTSStats
    values <- bracket (GL.genObject glGenQueries) (GL.deleteObject glDeleteQueries) $ \query -> forM [1 .. 90 :: Int] $ \_ -> do
      _ <- N.frameTime window
      N.begin window
      glBeginQuery GL_TIME_ELAPSED query
      cpuStart <- getCPUTime
      start <- getMonotonicTimeNSec
      draw
      end <- getMonotonicTimeNSec
      cpuEnd <- getCPUTime
      glEndQuery GL_TIME_ELAPSED
      gpu <- alloca $ \out -> glGetQueryObjectui64v query GL_QUERY_RESULT out >> peek out
      N.end window
      pure (fromIntegral (end - start) / 1e6, fromIntegral (cpuEnd - cpuStart) / 1e9, fromIntegral gpu / 1e6 :: Double)
    performGC
    after <- getRTSStats
    errors <- N.nativeError window
    unless (null errors) (ioError (userError errors))
    GL.checkGL "moving animation profile"
    stats <- readIORef (R.frameStats renderer)
    putStrLn ("draw_counts,animation-lab," ++ show stats)
    let wall = [w | (w, _, _) <- values]; cpu = [c | (_, c, _) <- values]; gpu = [g | (_, _, g) <- values]
    printf "draw,animation-lab,submission_mean_ms,%.4f,submission_p95_ms,%.4f,cpu_mean_ms,%.4f,gpu_mean_ms,%.4f,gpu_p95_ms,%.4f,frame_alloc_bytes,%.0f\n" (mean wall) (p95 wall) (mean cpu) (mean gpu) (p95 gpu) (fromIntegral (allocated_bytes after - allocated_bytes before) / 90 :: Double)

stress :: Int -> Project
stress count = (emptyProject ("Stress " ++ show count)) {world = foldl add emptyWorld [1 .. count], camera = Camera (V3 30 35 45) (V3 0 0 0) 45}
  where
    add w i =
      let (e, next) = spawnMesh "Cube" "cube" w
          x = fromIntegral (i `mod` 64) - 32
          z = fromIntegral (i `div` 64) - 32
       in next {transforms = IM.insert e (identity {position = V3 x 0 z, scale = V3 0.4 0.4 0.4}) (transforms next)}

-- Eight copies of the real level's resolved geometry/materials. This is a
-- rendering workload, not eight competing game/script/physics instances.
expandLevel :: Project -> Project
expandLevel p = p {world = emptyWorld {nextId = 8 * stride, names = combine names, transforms = IM.unions [IM.mapKeysMonotonic (+ copy * stride) (IM.map (offset copy) (transforms w)) | copy <- [0 .. 7]], meshes = combine meshes, materials = combine materials, lights = lights w}, camera = Camera (V3 72 60 95) (V3 48 0 20) 55}
  where
    w = world p
    stride = nextId w
    combine field = IM.unions [IM.mapKeysMonotonic (+ copy * stride) (field w) | copy <- [0 .. 7]]
    offset copy t = let V3 x y z = position t in t {position = V3 (x + fromIntegral (copy `mod` 4) * 32) y (z + fromIntegral (copy `div` 4) * 45)}
