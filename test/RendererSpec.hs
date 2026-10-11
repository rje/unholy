module Main where

import qualified Unholy.Input as I
import qualified Data.IntSet as S
import qualified Unholy.Audio as Audio
import qualified Unholy.Gizmo as Gizmo
import Unholy.Types
import Unholy.Project
import qualified Unholy.Native as N
import qualified Unholy.Renderer as R
import qualified Unholy.Renderer.GL as G
import qualified Unholy.Renderer.Camera as C
import qualified Unholy.Renderer.Assets as A
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Graphics.GL
import Data.IORef
import Foreign
import Control.Monad (unless, forM_, replicateM_)
import Control.Exception (try, IOException)
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

render :: N.Window -> Project -> (Float,Float,Float,Float) -> Bool -> IO ()
render window p rect edit = do
  _<-N.frameTime window
  N.begin window
  -- A batch queued before the renderer and another after it test both edges.
  N.rect window 0 0 20 20 3
  _<-N.drawScene window "." p rect edit True (Just 2)
  N.text window 20 20 18 "Haskell renderer verification" 0
  N.end window

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
  N.withWindow "Unholy / renderer checks" "assets" $ \window -> N.withRenderer window (checks window)
  putStrLn "All GPU renderer checks passed."

checks :: N.Window -> R.Renderer -> IO ()
checks window r = do
  createDirectoryIfMissing True "build/verification/haskell-renderer"
  let rect=(230,70,640,480)
  originalUI<-readIORef (N.uiState window)
  let typing=originalUI {N.focusedText=Just (44,"-"),N.frameInput=I.emptyFrame {I.heldKeys=S.singleton 87}}
  writeIORef (N.uiState window) typing
  suppressed<-N.key window 87 False
  typed<-N.inputs window
  N.begin window
  incomplete<-N.textbox window 44 10 10 100 30 "1.0"
  N.end window
  assert "Haskell text focus must suppress game/editor shortcuts" (not suppressed && typed==[0,0,0,0,0])
  assert "Incomplete numeric editing buffer was lost" (incomplete=="-")
  N.clearFocus window
  released<-N.key window 87 False
  assert "Clearing text focus must restore shortcuts" released
  writeIORef (N.uiState window) originalUI
  pass "Haskell text focus, persistent edit buffers and shortcut suppression"
  Audio.play (N.audio window) "projects/marble-run/assets/audio/ambience.wav" 0.15 1 True
  Audio.play (N.audio window) "projects/marble-run/assets/audio/jump.wav" 0.25 1 False
  Audio.play (N.audio window) "projects/marble-run/assets/audio/jump.wav" 0.25 1.2 False
  replicateM_ 12 (render window fixture rect False)
  (mixed,peak)<-Audio.mixStats
  assert ("audio device produced no signal: "++show (mixed,peak)) (mixed>0 && peak>0.001)
  N.pauseAudio window True;replicateM_ 3 (render window fixture rect False)
  mapM_ Audio.toggleMute (N.audio window);N.pauseAudio window False;mapM_ Audio.toggleMute (N.audio window)
  mapM_ Audio.stopOn (N.audio window)
  pass ("Haskell-managed sample sounds reach the native mixer: "++show mixed++" frames, peak "++show peak)
  forM_ [Gizmo.Move,Gizmo.Rotate,Gizmo.Scale] $ \mode->do
    R.setGizmoMode r (Just mode)
    render window fixture rect True
    N.screenshot window ("build/verification/haskell-renderer/gizmo-"++show mode++".png")
  R.setGizmoMode r Nothing
  pass "Haskell move/rotate/scale gizmos draw in the real GPU viewport"
  render window fixture rect False
  red<-pixelAtCenter r
  let blueFixture=fixture {world=(world fixture) {materials=IM.adjust (\m->m {albedo=V3 0.05 0.05 0.9}) 2 (materials (world fixture))}}
  render window blueFixture rect False
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

  N.begin window
  N.rect window 0 0 30 30 3
  R.flushUI
  R.setGizmoMode r (Just Gizmo.Rotate)
  before<-G.captureState
  _<-N.drawScene window "." fixture rect True True (Just 2)
  after<-G.captureState
  R.setGizmoMode r Nothing
  assert ("UI context state changed: "++show (before,after)) (before==after)
  N.end window
  pass "editor overlays preserve the UI's framebuffer, shader, buffers and GL state"

  models<-readIORef (R.meshCache r)
  replicateM_ 3 (render window fixture rect False)
  warmed<-readIORef (R.meshCache r)
  let ids=map (G.vertexArray . A.meshBuffer) . concatMap A.modelMeshes . M.elems
  assert "cached geometry was re-uploaded" (ids models==ids warmed)
  old<-currentTarget r
  render window fixture (230,70,511,333) False
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
    outcome<-try (N.close window) :: IO (Either IOException ())
    putMVar result outcome
  wrongThread<-takeMVar result
  assert "renderer allowed shutdown from a different OS thread" (either (const True) (const False) wrongThread)
  render window fixture rect False
  pass "GPU access from another thread is rejected without destroying the window"

  -- Picking uses retained CPU triangles after the importer has been released.
  let cam=camera fixture
      mouse=C.Mouse 550 310 0 0 0 False False True True
      ray=C.screenRay rect cam mouse
  draws<-mapM (\(e,t,m,mat)->do model<-R.getModel r (geometry m);pure (R.Draw e model (C.modelMatrix t (A.importTransform model)) mat (R.whiteTexture r))) (renderables (world fixture))
  assert "nearest visible mesh was not picked" (R.pick ray draws Nothing==Just 2)
  pass "editor picking intersects imported geometry and selects the nearest entity"

  forM_ ["material-lab","marble-run","knockdown"] $ \name -> do
    N.unloadAssets window
    p<-loadProject ("projects/"++name++"/project.hj.json") >>= either (ioError . userError) pure
    replicateM_ 2 $ do
      N.begin window
      _<-N.drawScene window ("projects/"++name) p (230,70,800,600) False False Nothing
      N.text window 20 20 18 name 0
      N.end window
    err<-N.nativeError window
    assert (name++": "++err) (null err)
    N.screenshot window ("build/verification/haskell-renderer/check-"++name++".png")
  pass "all three sample projects load and draw through the Haskell renderer"

  loadedModels<-readIORef (R.meshCache r)
  textures<-readIORef (R.textureCache r)
  N.unloadAssets window
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
  N.unloadAssets window
  R.close r
  glIsTexture (R.whiteTexture r) >>= assert "white texture leaked on close" . (==GL_FALSE)
  glIsFramebuffer (G.framebuffer viewport) >>= assert "viewport leaked on close" . (==GL_FALSE)
  glIsFramebuffer (G.framebuffer (R.shadowTarget r)) >>= assert "shadow framebuffer leaked on close" . (==GL_FALSE)
  forM_ [R.pbrProgram r,R.shadowProgram r,R.screenProgram r,R.lineProgram r] $ \p -> glIsProgram (G.programId p) >>= assert "shader program leaked on close" . (==GL_FALSE)
  R.close r
  G.checkGL "Idempotent renderer disposal"
  pass "renderer shutdown releases its GPU resources before window destruction"
