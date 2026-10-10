module Main where

import qualified HJ.Audio as Audio
import HJ.Types
import HJ.Project
import qualified HJ.Native as N
import qualified HJ.Renderer as R
import qualified HJ.Renderer.GL as G
import qualified HJ.Renderer.Camera as C
import qualified HJ.Renderer.Assets as A
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Graphics.GL
import Data.IORef
import Foreign
import Control.Monad (unless, forM_, replicateM_)
import Control.Exception (finally, try, IOException)
import Control.Concurrent (runInBoundThread, forkIO, newEmptyMVar, putMVar, takeMVar)
import System.Directory (createDirectoryIfMissing, makeAbsolute)

assert :: String -> Bool -> IO ()
assert label okay = unless okay (ioError (userError label))
pass :: String -> IO ()
pass = putStrLn . ("PASS "++)

fixture :: Project
fixture = (emptyProject "Renderer calibration") {world=w,camera=Camera (V3 0 2 6) (V3 0 0.5 0) 45,environment=defaultEnvironment {sunShadows=True}}
  where
    (_,w0)=spawnMesh "Floor" "cube" emptyWorld
    (_,w1)=spawnMesh "Sphere" "sphere" w0
    w=w1 {transforms=IM.fromList [(1,Transform (V3 0 (-0.5) 0) (V3 0 0 0) (V3 8 1 8)),(2,identity {position=V3 0 0.5 0})],materials=IM.insert 2 (Material (V3 0.9 0.05 0.05) 0 0.4 0) (materials w1)}

render :: Project -> (Float,Float,Float,Float) -> Bool -> IO ()
render p rect edit = do
  _<-N.frameTime
  N.begin
  -- A batch queued before the renderer and another after it test both edges.
  N.rect 0 0 20 20 3
  _<-N.drawScene "." p rect edit True (Just 2)
  N.text 20 20 18 "Haskell renderer verification" 0
  N.end

currentTarget :: R.Renderer -> IO G.Target
currentTarget r = readIORef (R.viewportTarget r) >>= maybe (ioError (userError "Missing viewport")) pure
pixelAtCenter :: R.Renderer -> IO [Word8]
pixelAtCenter r = G.preservingState $ \_ -> do
  t<-currentTarget r
  glBindFramebuffer GL_READ_FRAMEBUFFER (G.framebuffer t)
  allocaArray 4 $ \p -> do
    glReadPixels (fromIntegral (G.targetWidth t `div` 2)) (fromIntegral (G.targetHeight t `div` 2)) 1 1 GL_RGBA GL_UNSIGNED_BYTE p
    peekArray 4 p

main :: IO ()
main = runInBoundThread $ do
  initialized<-N.initWindow "HJ / renderer checks" "assets"
  unless initialized (N.nativeError >>= ioError . userError)
  (N.withRenderer checks) `finally` N.close
  putStrLn "All GPU renderer checks passed."

checks :: R.Renderer -> IO ()
checks r = do
  createDirectoryIfMissing True "build/verification/haskell-renderer"
  let rect=(230,70,640,480)
  Audio.play "projects/marble-run/assets/audio/ambience.wav" 0.15 1 True
  Audio.play "projects/marble-run/assets/audio/jump.wav" 0.25 1 False
  Audio.play "projects/marble-run/assets/audio/jump.wav" 0.25 1.2 False
  replicateM_ 12 (render fixture rect False)
  (mixed,peak)<-Audio.mixStats
  assert ("audio device produced no signal: "++show (mixed,peak)) (mixed>0 && peak>0.001)
  Audio.pause True;replicateM_ 3 (render fixture rect False)
  Audio.toggleMute;Audio.pause False;Audio.toggleMute
  Audio.stop
  pass ("Haskell-managed sample sounds reach the native mixer: "++show mixed++" frames, peak "++show peak)
  render fixture rect False
  red<-pixelAtCenter r
  let blueFixture=fixture {world=(world fixture) {materials=IM.adjust (\m->m {albedo=V3 0.05 0.05 0.9}) 2 (materials (world fixture))}}
  render blueFixture rect False
  blue<-pixelAtCenter r
  assert ("PBR draw/uniforms did not affect pixels: "++show (red,blue)) (fromIntegral (red!!0)>fromIntegral (red!!2)+(30::Int) && fromIntegral (blue!!2)>fromIntegral (blue!!0)+(30::Int))
  pass "direct mesh drawing and live PBR material changes affect GPU pixels"

  G.preservingState $ \_ -> do
    let shadow=R.shadowTarget r
    glBindFramebuffer GL_READ_FRAMEBUFFER (G.framebuffer shadow)
    let n=G.targetWidth shadow*G.targetHeight shadow
    storage<-mallocForeignPtrArray n
    withForeignPtr storage $ \p -> do
      glReadPixels 0 0 2048 2048 GL_DEPTH_COMPONENT GL_FLOAT p
    let values=VS.unsafeFromForeignPtr0 storage n :: VS.Vector Float
    assert "shadow pass did not write depth" (VS.minimum values<0.2 && VS.maximum values>0.99)
  pass "sun shadow pass writes a complete depth framebuffer"

  N.begin
  N.rect 0 0 30 30 3
  R.flushUI
  before<-G.captureState
  _<-N.drawScene "." fixture rect True True (Just 2)
  after<-G.captureState
  assert ("UI context state changed: "++show (before,after)) (before==after)
  N.end
  pass "editor overlays preserve the UI's framebuffer, shader, buffers and GL state"

  models<-readIORef (R.meshCache r)
  replicateM_ 3 (render fixture rect False)
  warmed<-readIORef (R.meshCache r)
  let ids=map (G.vertexArray . A.meshBuffer) . concatMap A.modelMeshes . M.elems
  assert "cached geometry was re-uploaded" (ids models==ids warmed)
  old<-currentTarget r
  render fixture (230,70,511,333) False
  resized<-currentTarget r
  let ratioX=fromIntegral (G.targetWidth old)/640::Float
      ratioY=fromIntegral (G.targetHeight old)/480::Float
  assert "viewport dimensions did not resize" (G.targetWidth resized==round (511*ratioX) && G.targetHeight resized==round (333*ratioY))
  oldFbo<-glIsFramebuffer (G.framebuffer old)
  oldColor<-glIsTexture (G.colorTexture old)
  assert "old viewport resources leaked" (oldFbo==GL_FALSE && oldColor==GL_FALSE)
  pass "warm frames reuse mesh buffers; resizing deletes the old viewport"

  createDirectoryIfMissing True "test-output"
  texturePath<-makeAbsolute "projects/marble-run/assets/textures/temple-stone.png"
  -- Embedded importer materials must survive release of the native model.
  writeFile "test-output/import.mtl" ("newmtl stone\nKd 1 1 1\nmap_Kd "++texturePath++"\n")
  writeFile "test-output/import.obj" "mtllib import.mtl\no surface\nv -1 -1 0\nv 1 -1 0\nv 0 1 0\nvt 0 0\nvt 1 0\nvt 0.5 1\nvn 0 0 1\nusemtl stone\nf 1/1/1 2/2/1 3/3/1\n"
  G.preservingState $ \_ -> do
    imported<-A.loadModel "test-output/import.obj"
    let importedTextures=[tex | mesh<-A.modelMeshes imported,Just tex<-[A.importedTexture mesh]]
    assert "imported albedo texture was lost" (not (null importedTextures))
    forM_ importedTextures $ \tex -> glIsTexture tex >>= assert "importer release deleted Haskell texture" . (==GL_TRUE)
    A.deleteModel imported
    forM_ importedTextures $ \tex -> glIsTexture tex >>= assert "imported texture leaked" . (==GL_FALSE)
  pass "imported material textures are copied, retained and released by Haskell"

  result<-newEmptyMVar
  _<-forkIO $ do
    outcome<-try N.close :: IO (Either IOException ())
    putMVar result outcome
  wrongThread<-takeMVar result
  assert "renderer allowed shutdown from a different OS thread" (either (const True) (const False) wrongThread)
  render fixture rect False
  pass "GPU access from another thread is rejected without destroying the window"

  -- Picking uses retained CPU triangles after the importer has been released.
  let cam=camera fixture
      mouse=C.Mouse 550 310 0 0 0 False False True True
      ray=C.screenRay rect cam mouse
  draws<-mapM (\(e,t,m,mat)->do model<-R.getModel r (geometry m);pure (R.Draw e model (C.modelMatrix t (A.importTransform model)) mat (R.whiteTexture r))) (renderables (world fixture))
  assert "nearest visible mesh was not picked" (R.pick ray draws Nothing==Just 2)
  pass "editor picking intersects imported geometry and selects the nearest entity"

  forM_ ["material-lab","marble-run","knockdown"] $ \name -> do
    N.unloadAssets
    p<-loadProject ("projects/"++name++"/project.hj.json") >>= either (ioError . userError) pure
    replicateM_ 2 $ do
      N.begin
      _<-N.drawScene ("projects/"++name) p (230,70,800,600) False False Nothing
      N.text 20 20 18 name 0
      N.end
    err<-N.nativeError
    assert (name++": "++err) (null err)
    N.screenshot ("build/verification/haskell-renderer/check-"++name++".png")
  pass "all three sample projects load and draw through the Haskell renderer"

  loadedModels<-readIORef (R.meshCache r)
  textures<-readIORef (R.textureCache r)
  N.unloadAssets
  forM_ (ids loadedModels) $ \vao -> glIsVertexArray vao >>= assert "project switch leaked a mesh VAO" . (==GL_FALSE)
  forM_ (filter (/=R.whiteTexture r) (M.elems textures)) $ \tex -> glIsTexture tex >>= assert "project switch leaked a texture" . (==GL_FALSE)
  pass "project switching releases cached GPU assets"

  _<-R.getModel r "missing-mesh-for-test.obj"
  _<-R.getTexture r "missing-texture-for-test.png"
  errors<-R.takeErrors r
  assert "missing asset diagnostics/fallbacks failed" (length errors==2)
  bad<-try (G.compileProgram "broken.vs" "#version 330\nthis is invalid" "test.fs" "#version 330\nout vec4 c;void main(){c=vec4(1);}") :: IO (Either IOException G.Program)
  case bad of Left _->pure ();Right p->glDeleteProgram (G.programId p) >> assert "invalid shader compiled" False
  G.checkGL "GPU verification"
  pass "missing assets fall back and shader compilation reports errors"

  -- Keep the context alive long enough to prove renderer cleanup, rather than
  -- relying on the OS reclaiming resources when the window closes.
  viewport<-currentTarget r
  N.unloadAssets
  R.close r
  glIsTexture (R.whiteTexture r) >>= assert "white texture leaked on close" . (==GL_FALSE)
  glIsFramebuffer (G.framebuffer viewport) >>= assert "viewport leaked on close" . (==GL_FALSE)
  glIsFramebuffer (G.framebuffer (R.shadowTarget r)) >>= assert "shadow framebuffer leaked on close" . (==GL_FALSE)
  forM_ [R.pbrProgram r,R.shadowProgram r,R.screenProgram r,R.lineProgram r] $ \p -> glIsProgram (G.programId p) >>= assert "shader program leaked on close" . (==GL_FALSE)
  R.close r
  G.checkGL "Idempotent renderer disposal"
  pass "renderer shutdown releases its GPU resources before window destruction"
