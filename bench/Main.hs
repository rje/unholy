module Main where

import Unholy
import qualified Unholy.Native as N
import qualified Unholy.Renderer.GL as GL
import qualified Unholy.Renderer as R
import qualified Data.IntMap.Strict as IM
import PulseCourier (pulseCourier)
import CircuitBreaker (circuitBreaker)
import Graphics.GL
import Foreign
import Foreign.C (peekCString)
import Control.Exception (finally,evaluate,bracket)
import Control.Concurrent (runInBoundThread)
import Control.Monad (forM_,forM,replicateM_,when,unless)
import Data.IORef
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.Mem (performGC)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Info (os,arch)
import Text.Printf (printf)

main :: IO ()
main=runInBoundThread $ do
  args<-getArgs
  enabled<-getRTSStatsEnabled
  when (not enabled) (ioError (userError "Run with +RTS -T to collect allocations"))
  printf "platform,%s,%s\n" os arch
  putStrLn "CPU timings and allocation exclude startup; warm graphics use the actual renderer."
  forM_ [1000,4000] $ \count->do
    let p=stress count
    evaluate (validateProject p) >>= either (ioError.userError) pure
    measure ("validate-"++show count) 400 $ \i->do
      let w=world p;changed=w {transforms=IM.adjust (\t->t {position=V3 (fromIntegral i/1000) 0 0}) 1 (transforms w)}
      value<-evaluate (validateProject p {world=changed})
      either (ioError.userError) pure value
  samples<-forM ["marble-run","pulse-courier","circuit-breaker","material-gallery"] $ \name->do
    p<-loadProject ("projects/"++name++"/project.hj.json") >>= either (ioError.userError) pure
    pure (name,p)
  forM_ (take 3 samples) $ \(name,p)->do
    (runtime,started,_)<-startRuntimeWith [pulseCourier,circuitBreaker] Nothing ("projects/"++name) p
    current<-newIORef (runtime,started)
    let tick :: Int -> IO ()
        tick _=do
          (r,scene)<-readIORef current
          (next,updated,_,_)<-stepGame (1/120) (Input 0 0 False False False) r scene
          writeIORef current (next,updated)
    (replicateM_ 240 (tick 0) >> measure ("tick-"++name) 240 tick) `finally` stopRuntime runtime
  when ("--gpu" `elem` args) $ N.withWindow "Unholy / measured renderer" "assets" $ \window->do
    renderer<-N.withRenderer window pure
    device<-glGetString GL_RENDERER >>= peekCString . castPtr
    putStrLn ("gpu_device,"++device)
    forM_ (samples++[("stress-1000",stress 1000),("stress-4000",stress 4000)]) $ \(name,p)->do
      N.unloadAssets window
      let draw=do _<-N.drawScene window (if take 7 name=="stress-" then "." else "projects/"++name) p (0,0,1024,768) False False Nothing;pure ()
          frame=do _<-N.frameTime window;N.begin window;draw;N.end window
      replicateM_ 20 frame
      assetErrors<-N.nativeError window
      unless (null assetErrors) (ioError (userError assetErrors))
      viewport<-readIORef (R.viewportTarget renderer)
      forM_ viewport $ \t->printf "viewport,%s,%d,%d\n" name (GL.targetWidth t) (GL.targetHeight t)
      performGC
      before<-getRTSStats
      values<-bracket (GL.genObject glGenQueries) (GL.deleteObject glDeleteQueries) $ \query->forM [1..120::Int] $ \_->do
        _<-N.frameTime window;N.begin window
        glBeginQuery GL_TIME_ELAPSED query
        cpuStart<-getCPUTime;start<-getMonotonicTimeNSec
        draw
        end<-getMonotonicTimeNSec;cpuEnd<-getCPUTime
        glEndQuery GL_TIME_ELAPSED
        gpu<-alloca $ \out->glGetQueryObjectui64v query GL_QUERY_RESULT out >> peek out
        N.end window
        pure (fromIntegral (end-start)/1e6,fromIntegral (cpuEnd-cpuStart)/1e9,fromIntegral gpu/1e6::Double)
      performGC
      after<-getRTSStats
      GL.checkGL "GPU timer query profile"
      measuredErrors<-N.nativeError window
      unless (null measuredErrors) (ioError (userError measuredErrors))
      let wall=[w | (w,_,_)<-values];cpu=[c | (_,c,_)<-values];gpu=[g | (_,_,g)<-values]
      printf "draw,%s,submission_mean_ms,%.4f,submission_p95_ms,%.4f,cpu_mean_ms,%.4f,gpu_mean_ms,%.4f,gpu_p95_ms,%.4f,frame_alloc_bytes,%.0f\n" name (mean wall) (p95 wall) (mean cpu) (mean gpu) (p95 gpu) (fromIntegral (allocated_bytes after-allocated_bytes before)/120::Double)

measure :: String -> Int -> (Int -> IO ()) -> IO ()
measure label count work=do
  performGC
  before<-getRTSStats
  cpuStart<-getCPUTime
  times<-forM [1..count] $ \i->do start<-getMonotonicTimeNSec;work i;end<-getMonotonicTimeNSec;pure (fromIntegral (end-start)/1e6::Double)
  cpuEnd<-getCPUTime
  performGC
  after<-getRTSStats
  printf "cpu,%s,mean_ms,%.4f,p95_ms,%.4f,cpu_mean_ms,%.4f,alloc_bytes,%.0f\n" label (mean times) (p95 times) (fromIntegral (cpuEnd-cpuStart)/1e9/fromIntegral count::Double) (fromIntegral (allocated_bytes after-allocated_bytes before)/fromIntegral count::Double)
mean :: [Double] -> Double
mean xs=sum xs/fromIntegral (length xs)
p95 :: [Double] -> Double
p95 xs=sort xs!!min (length xs-1) (length xs*95 `div` 100)

stress :: Int -> Project
stress count=(emptyProject ("Stress "++show count)) {world=foldl add emptyWorld [1..count],camera=Camera (V3 30 35 45) (V3 0 0 0) 45}
  where
    add w i=
      let (e,next)=spawnMesh "Cube" "cube" w
          x=fromIntegral (i `mod` 64)-32
          z=fromIntegral (i `div` 64)-32
      in next {transforms=IM.insert e (identity {position=V3 x 0 z,scale=V3 0.4 0.4 0.4}) (transforms next)}
