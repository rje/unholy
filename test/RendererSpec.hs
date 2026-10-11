module Main where

import PhysicsSpec (nodeFixture)
import Samples
import Unholy.Asset.CPU (loadTriangles)
import qualified Unholy.Input as I
import qualified Data.IntSet as S
import qualified Unholy.Audio as Audio
import qualified Unholy.Animation as Animation
import qualified Unholy.Asset.GLTF as Asset
import qualified Unholy.Gizmo as Gizmo
import qualified Unholy.Terrain as T
import Unholy.Types
import Unholy.Project
import qualified Unholy.Native as N
import qualified Unholy.Renderer as R
import qualified Unholy.Renderer.GL as G
import qualified Unholy.Renderer.Camera as C
import qualified Unholy.Renderer.Assets as A
import qualified Unholy.Renderer.GLTF as F
import qualified Unholy.Renderer.IBL as B
import qualified Linear as L
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Graphics.GL
import Data.IORef
import Foreign
import Control.Monad (unless, forM, forM_, replicateM_)
import Control.Exception (try, IOException, bracket)
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
  pixels<-forM ["external.gltf","data-uri.gltf","embedded.glb"] $ \name->
    G.preservingState $ \_->bracket (Asset.loadAsset ("test/fixtures/gltf/"++name) >>= A.uploadRig True) A.deleteModel $ \model->do
      let texture=maybe (error "Missing fixture base texture") id (A.importedTexture (head (A.modelMeshes model)))
      glActiveTexture GL_TEXTURE0
      glBindTexture GL_TEXTURE_2D texture
      allocaArray 4 $ \out->glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_BYTE out >> peekArray 4 out :: IO [Word8]
  assert "External, GLB and data-URI image decoding differs" (all (==head pixels) (tail pixels))
  pass "unified external/GLB/data-URI texture decoding before GPU upload"
  animatedRendering window r rect
  terrainRendering window r rect
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
  N.begin window
  outsideRange<-N.slider window 10 100 100 "Preserve authored angle" 270 (-180) 180
  N.end window
  assert "Unedited slider clamped an authored value" (outsideRange==270)
  released<-N.key window 87 False
  assert "Clearing text focus must restore shortcuts" released
  writeIORef (N.uiState window) originalUI
  pass "Haskell text focus, persistent edit buffers and shortcut suppression"
  controllerReplay window
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
  environmentLighting window r rect
  scaling window r rect
  render window fixture rect False

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
  importedMaterials window r rect
  colliderImportAlignment

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
  draws<-mapM (\(e,t,m,mat)->do model<-R.getModel r (geometry m);pure (R.Draw e model (C.modelMatrix t (A.importTransform model)) mat (R.whiteTexture r) False)) (renderables (world fixture))
  assert "nearest visible mesh was not picked" (R.pick ray draws Nothing==Just 2)
  pass "editor picking intersects imported geometry and selects the nearest entity"

  forM_ sampleProjects $ \name -> do
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
  pass ("all "++show (length sampleProjects)++" sample projects load and draw through the Haskell renderer")

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
  glIsBuffer (R.instanceBuffer r) >>= assert "instance stream buffer leaked on close" . (==GL_FALSE)
  glIsFramebuffer (G.framebuffer viewport) >>= assert "viewport leaked on close" . (==GL_FALSE)
  glIsFramebuffer (G.framebuffer (R.shadowTarget r)) >>= assert "shadow framebuffer leaked on close" . (==GL_FALSE)
  forM_ [R.pbrProgram r,R.shadowProgram r,R.screenProgram r,R.lineProgram r,R.iblProgram r] $ \p -> glIsProgram (G.programId p) >>= assert "shader program leaked on close" . (==GL_FALSE)
  R.close r
  G.checkGL "Idempotent renderer disposal"
  pass "renderer shutdown releases its GPU resources before window destruction"

targetPixels :: R.Renderer -> IO (VS.Vector Word8)
targetPixels r=G.preservingState $ \_->do
  t<-currentTarget r
  let count=G.targetWidth t*G.targetHeight t*4
  bytes<-mallocForeignPtrArray count
  glBindFramebuffer GL_READ_FRAMEBUFFER (G.framebuffer t)
  withForeignPtr bytes $ \p->glReadPixels 0 0 (fromIntegral (G.targetWidth t)) (fromIntegral (G.targetHeight t)) GL_RGBA GL_UNSIGNED_BYTE p
  pure (VS.unsafeFromForeignPtr0 bytes count)

scaling :: N.Window -> R.Renderer -> (Float,Float,Float,Float) -> IO ()
scaling window r rect=do
  let (_,w)=foldl (\(_,current) i->spawnMesh ("Batch "++show i) "cube" current) (0,emptyWorld) [1..6::Int]
      scene=(emptyProject "Instancing and off-camera shadow") {camera=Camera (V3 0 3 8) (V3 0 0 (-1)) 50,environment=defaultEnvironment {sunShadows=True,sunDirection=V3 1 (-0.5) 0},world=w {transforms=IM.fromList
        [(1,identity {position=V3 0 (-0.5) 0,scale=V3 8 1 8})
        ,(2,Transform (V3 (-1) 0.5 0) (V3 12 20 15) (V3 0.7 1 1.3))
        ,(3,Transform (V3 1 0.5 0) (V3 12 (-30) 15) (V3 0.7 1 1.3))
        ,(4,Transform (V3 (-1) 0.5 (-2)) (V3 0 15 0) (V3 1.2 0.8 0.6))
        ,(5,Transform (V3 1 0.5 (-2)) (V3 0 (-15) 0) (V3 1.2 0.8 0.6))
        ,(6,identity {position=V3 (-8) 4 0,scale=V3 2 2 2})]
        ,materials=IM.fromList [(i,if i `elem` [2,3,6] then Material (V3 0.9 0.1 0.1) 0.3 0.4 0 else if i `elem` [4,5] then Material (V3 0.1 0.1 0.9) 0.7 0.25 0 else defaultMaterial) | i<-[1..6]]}}
      difference a b=VS.zipWith (\x y->abs (fromIntegral x-fromIntegral y)::Int) a b
  writeIORef (R.instancingEnabled r) False
  writeIORef (R.cullingEnabled r) False
  render window scene rect False
  reference<-targetPixels r
  writeIORef (R.instancingEnabled r) True
  writeIORef (R.cullingEnabled r) True
  render window scene rect False
  batched<-targetPixels r
  counts<-readIORef (R.frameStats r)
  let errors=difference reference batched
      meanError=fromIntegral (VS.sum errors)/fromIntegral (VS.length errors)::Double
  assert ("instancing/culling changed visible pixels: "++show meanError) (meanError<0.35)
  assert ("unexpected visible/batched counts: "++show counts) (R.totalMeshes counts==6 && R.visibleMeshes counts==5 && R.colorCalls counts==3 && R.shadowCalls counts==1)
  render window scene {world=destroy 6 (world scene)} rect False
  noCaster<-targetPixels r
  assert "camera-invisible caster did not affect visible shadows" (VS.sum (difference batched noCaster)>10000)
  poses<-readIORef (R.poseCache r)
  let edited=scene {world=(world scene) {transforms=IM.adjust (\t->t {position=V3 (-0.4) 1 0}) 2 (transforms (world scene)),meshes=IM.adjust (\m->m {geometry="torus"}) 2 (meshes (world scene))}}
  render window edited rect False
  updated<-readIORef (R.poseCache r)
  assert "edited pose/mesh retained stale matrices" (R.cachedMatrix (updated IM.! 2)/=R.cachedMatrix (poses IM.! 2) && R.cachedGeometry (updated IM.! 2)=="torus")
  N.unloadAssets window
  readIORef (R.poseCache r) >>= assert "project switch retained prepared poses" . IM.null
  pass "instanced nonuniform meshes match scalar/unculled pixels; off-camera shadows and edited poses survive"

importedMaterials :: N.Window -> R.Renderer -> (Float,Float,Float,Float) -> IO ()
importedMaterials window r rect=do
  let path="./projects/material-gallery/assets/cards.gltf"
      (e,w)=spawnMesh "Imported cards" "projects/material-gallery/assets/cards.gltf" emptyWorld
      authored=w {meshes=IM.adjust (\m->m {useImportedMaterials=True}) e (meshes w)}
      scene=(emptyProject "glTF") {world=authored,environment=defaultEnvironment {sunPower=0,ambient=1,exposure=1,fogDensity=0,sunShadows=False}}
      card i p=p {camera=Camera (V3 ((fromIntegral i-2.5)*2.4) 0 6) (V3 ((fromIntegral i-2.5)*2.4) 0 0) 45}
      at i p=render window (card (i::Int) p) rect False >> pixelAtCenter r
      near a b=abs (a-b)<0.00001
  loaded<-R.getModel r path
  let infos=[A.materialInfo m | mesh<-A.modelMeshes loaded,Just m<-[A.importedMaterial mesh]]
      textures=concatMap A.meshTextures (A.modelMeshes loaded)
  assert "Submesh material count" (length infos==6)
  assert "Float-only glTF factors lost or quantized" (near (F.metalFactor (head infos)) 0.1 && near (F.roughFactor (head infos)) 0.24 && F.baseFactor (head infos)==L.V3 0.65 0.035 0.06)
  forM_ textures $ \tex->glIsTexture tex >>= assert "Haskell texture did not survive importer release" . (==GL_TRUE)
  let maps=A.materialTextures (maybe (error "Missing material") id (A.importedMaterial (A.modelMeshes loaded!!2)))
  G.preservingState $ \_->do
    forM_ [(0,GL_SRGB8_ALPHA8),(2,GL_RGBA8),(3,GL_RGBA8)] $ \(slot,expected)->do
      glBindTexture GL_TEXTURE_2D (maybe 0 id (maps!!slot))
      actual<-alloca $ \out->glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT out >> peek out
      assert "Material texture colorspace incorrect" (actual==fromIntegral expected)
    forM_ [(2,20),(3,200)] $ \(slot,expected)->do
      glBindTexture GL_TEXTURE_2D (maybe 0 id (maps!!slot))
      allocaArray (64*64*4) $ \out->do
        glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_BYTE out
        value<-peek out :: IO Word8
        assert "Packed metallic/roughness channels swapped or gamma decoded" (value==expected)
  red<-at 0 scene;blue<-at 1 scene
  assert ("Submesh GPU colors failed "++show (red,blue)) (red!!0>red!!2+35 && blue!!2>blue!!0+35)
  let (copy,twice)=duplicate e authored
      (overrideCopy,three)=duplicate e twice
      grouped=scene {camera=Camera (V3 0 3 18) (V3 0 3 0) 45,world=three
        {transforms=IM.adjust (\t->t {position=V3 0 6 0}) overrideCopy (IM.adjust (\t->t {position=V3 0 3 0}) copy (transforms three))
        ,meshes=IM.adjust (\m->m {useImportedMaterials=False}) overrideCopy (meshes three)
        ,materials=IM.insert overrideCopy (Material (V3 0.02 0.9 0.02) 0 0.4 0) (materials three)}}
  writeIORef (R.instancingEnabled r) False
  writeIORef (R.cullingEnabled r) False
  render window grouped rect False
  baseline<-targetPixels r
  writeIORef (R.instancingEnabled r) True
  writeIORef (R.cullingEnabled r) True
  render window grouped rect False
  optimized<-targetPixels r
  counts<-readIORef (R.frameStats r)
  let errorSum=VS.sum (VS.zipWith (\a b->abs (fromIntegral a-fromIntegral b)::Int) baseline optimized)
  assert "imported slots/entity overrides bled across batches" (fromIntegral errorSum/fromIntegral (VS.length baseline)<(0.35::Double) && R.visibleMeshes counts==18 && R.colorCalls counts==12)
  actual<-at 5 scene
  let linear=((192/255+0.055)/1.055)**2.4 :: Double
      expected factor=let x=linear*factor*0.5;m=(x*(2.51*x+0.03))/(x*(2.43*x+0.59)+0.14) in round (max 0 (min 1 m)**(1/2.2)*255)::Int
  assert ("sRGB must decode before linear factor multiplication "++show actual) (and (zipWith (\got wanted->abs (fromIntegral got-wanted)<=3) (take 3 actual) (map expected [0.3,0.12,0.65])))
  let lit=scene {environment=(environment scene) {sunPower=3,ambient=0.1}}
  normal<-at 3 lit
  modifyIORef' (R.meshCache r) (M.adjust (\model->model {A.modelMeshes=map (\mesh->mesh {A.importedMaterial=fmap (\m->m {A.materialInfo=(A.materialInfo m) {F.normalScale=0}}) (A.importedMaterial mesh)}) (A.modelMeshes model)}) path)
  flat<-at 3 lit
  assert ("Directional normal map did not affect real GPU pixels "++show (normal,flat)) (maximum (zipWith (\a b->abs (fromIntegral a-fromIntegral b)::Int) normal flat)>10)
  let variant name=lit {world=authored {meshes=IM.adjust (\m->m {geometry="projects/material-gallery/assets/"++name++".gltf"}) e (meshes authored)}}
  mirrored<-at 3 (variant "normal-mirror")
  degenerate<-at 3 (variant "normal-degenerate")
  assert ("Mirrored UV normal handedness lost "++show (normal,mirrored)) (maximum (zipWith (\a b->fromIntegral a-fromIntegral b::Int) normal mirrored)>20)
  assert "Degenerate UVs must fall back to the geometric normal" (maximum (zipWith (\a b->abs (fromIntegral a-fromIntegral b)::Int) flat degenerate)<=2)
  let override=scene {world=authored {meshes=IM.adjust (\m->m {useImportedMaterials=False}) e (meshes authored),materials=IM.insert e (Material (V3 0.02 0.9 0.02) 0 0.4 0) (materials authored)}}
  overridden<-at 0 override
  assert "Explicit entity override lost" (overridden!!1>overridden!!0+50 && overridden!!1>overridden!!2+50)
  saved<-saveProject "test-output/material-override.hj.json" override
  assert "Override save failed" (saved==Right ())
  reopened<-loadProject "test-output/material-override.hj.json"
  assert "Override policy did not round-trip" (reopened==Right override)
  N.unloadAssets window
  forM_ textures $ \tex->glIsTexture tex >>= assert "Imported material texture leaked" . (==GL_FALSE)
  forM_ [path,"projects/material-gallery/assets/cards.glb"] $ \file->do
    again<-R.getModel r file
    assert "Repeated import/GLB material metadata failed" (length (A.modelMeshes again)==6)
    let owned=concatMap A.meshTextures (A.modelMeshes again)
    N.unloadAssets window
    forM_ owned $ \tex->glIsTexture tex >>= assert "Repeated import texture leaked" . (==GL_FALSE)
  pass "static glTF slots, float factors, sRGB/data maps, normal pixels, override persistence and GPU lifetime"

-- Read linear convolution results, independent of tone mapping and screenshots.
cubePixels :: GLuint -> Int -> Int -> Int -> IO (VS.Vector Float)
cubePixels tex face level side=G.preservingState $ \_->do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_CUBE_MAP tex
  storage<-mallocForeignPtrArray (side*side*3)
  withForeignPtr storage $ \p->glGetTexImage (GL_TEXTURE_CUBE_MAP_POSITIVE_X+fromIntegral face) (fromIntegral level) GL_RGB GL_FLOAT p
  pure (VS.unsafeFromForeignPtr0 storage (side*side*3))

environmentLighting :: N.Window -> R.Renderer -> (Float,Float,Float,Float) -> IO ()
environmentLighting window r rect=do
  decoded<-B.loadHDR "projects/material-gallery/assets/studio.hdr"
  assert "Radiance decoder clipped values above one" (VS.maximum (B.hdrPixels decoded)>10)
  let constant=B.HDR 64 32 (VS.generate (64*32*4) (\i->[2,0.4,0.1,1]!!(i `mod` 4)))
  malformed<-try (B.generateIBL (R.iblProgram r) (R.screenBuffer r) (B.HDR 64 32 (VS.singleton 1))) :: IO (Either IOException B.IBL)
  assert "Short HDR pixel buffer was accepted" (either (const True) (const False) malformed)
  uniform<-B.generateIBL (R.iblProgram r) (R.screenBuffer r) constant
  forM_ [0..5] $ \face->do
    diffuse<-cubePixels (B.irradianceTexture uniform) face 0 32
    assert "Constant environment irradiance normalization is incorrect" (VS.and (VS.imap (\i value->abs (value-[2,0.4,0.1]!!(i `mod` 3))<0.003) diffuse))
    forM_ [0,3,6] $ \level->do
      specular<-cubePixels (B.prefilterTexture uniform) face level (128 `div` (2^level))
      assert "Constant environment specular convolution is incorrect" (VS.and (VS.imap (\i value->abs (value-[2,0.4,0.1]!!(i `mod` 3))<0.003) specular))
  B.deleteIBL uniform
  let directional=B.HDR 64 32 (VS.generate (64*32*4) (\i->
        let (pixel,channel)=i `divMod` 4;(y,x)=pixel `divMod` 64
            upper=if y<16 then [0.3,0.02,0.01] else [0.01,0.02,0.3]
            patch=if abs (x-32)<5 && abs (y-9)<4 then 12 else 0
        in if channel==3 then 1 else upper!!channel+patch))
  maps<-B.generateIBL (R.iblProgram r) (R.screenBuffer r) directional
  top<-cubePixels (B.irradianceTexture maps) 2 0 32
  bottom<-cubePixels (B.irradianceTexture maps) 3 0 32
  let meanChannel channel values=VS.sum (VS.generate (VS.length values `div` 3) (\i->values VS.! (i*3+channel)))/fromIntegral (VS.length values `div` 3)
  assert "HDR top/bottom orientation flipped" (meanChannel 0 top>meanChannel 2 top+0.1 && meanChannel 2 bottom>meanChannel 0 bottom+0.1)
  sharp<-mapM (\face->cubePixels (B.prefilterTexture maps) face 0 128) [0..5]
  blurred<-mapM (\face->cubePixels (B.prefilterTexture maps) face 4 8) [0..5]
  assert "GGX roughness did not broaden the bright reflection" (maximum (map VS.maximum blurred)<maximum (map VS.maximum sharp)*0.7)
  B.deleteIBL maps
  let lit=fixture {environment=defaultEnvironment {environmentMap="projects/material-gallery/assets/studio.hdr",environmentIntensity=1,sunPower=0,ambient=0,fogDensity=0,sunShadows=False}}
      rotated p=p {environment=(environment p) {environmentRotation=90}}
      delta a b=VS.sum (VS.zipWith (\x y->abs (fromIntegral x-fromIntegral y)::Int) a b)
  render window lit rect False
  reflection<-targetPixels r
  interior<-pixelAtCenter r
  assert ("IBL did not light the sphere interior: "++show interior) (sum (map (fromIntegral :: Word8 -> Int) (take 3 interior))>60)
  retained<-readIORef (R.environmentCache r)
  render window (rotated lit) rect False
  rotatedReflection<-targetPixels r
  rotatedInterior<-pixelAtCenter r
  assert ("Sphere interior ignored environment rotation: "++show (interior,rotatedInterior)) (sum (zipWith (\a b->abs (fromIntegral a-fromIntegral b)::Int) (take 3 interior) (take 3 rotatedInterior))>5)
  again<-readIORef (R.environmentCache r)
  assert "Environment rotation rebuilt convolution textures" (retained==again)
  assert "Rotating HDR did not change surface reflections" (delta reflection rotatedReflection>100000)
  render window lit {world=emptyWorld} rect False
  sky<-targetPixels r
  render window (rotated lit) {world=emptyWorld} rect False
  rotatedSky<-targetPixels r
  assert "Rotating HDR did not change visible sky" (delta sky rotatedSky>100000)
  render window lit {world=emptyWorld,camera=Camera (V3 3 0 0) (V3 0 0 0) 45} rect False
  seam<-pixelAtCenter r
  assert ("Equirectangular longitude seam leaked distant bright mip lighting: "++show seam) (maximum (take 3 seam)<100)
  render window lit {environment=(environment lit) {environmentIntensity=0}} rect False
  zero<-pixelAtCenter r
  assert ("Zero environment intensity retained ambient light: "++show zero) (take 3 zero==[0,0,0])
  N.begin window
  R.flushUI
  before<-G.captureState
  _<-N.drawScene window "." lit rect False False Nothing
  after<-G.captureState
  N.end window
  assert "HDR draw changed shared 2D/cubemap GL state" (before==after)
  N.unloadAssets window
  forM_ retained $ \(_,owned)->forM_ owned $ \value->forM_ [B.radianceTexture value,B.irradianceTexture value,B.prefilterTexture value,B.brdfTexture value] $ \tex->
    glIsTexture tex >>= assert "Environment texture leaked on project switch" . (==GL_FALSE)
  render window fixture rect False
  pixelAtCenter r >>= assert "Gradient/ambient fallback broke" . (\pixel->pixel!!0>pixel!!2+30)
  pass "float HDR, normalized irradiance, GGX roughness/orientation, rotating sky/reflections, cached uniforms and environment lifetime"

colliderImportAlignment :: IO ()
colliderImportAlignment=G.preservingState $ \_->do
  (root,_)<-nodeFixture
  cpu<-loadTriangles (root++"/nodes.gltf")
  bracket (A.loadModel (root++"/nodes.gltf")) A.deleteModel $ \model->do
    let ray=C.Ray (L.V3 3 8 (2/3)) (L.V3 0 (-1) 0)
        expected=C.rayMesh ray (C.boundsOf cpu) cpu VS.empty
        hits=[distance | mesh<-A.modelMeshes model,Just distance<-[C.rayMesh (C.objectRay (A.importTransform model) ray) (A.meshBounds mesh) (A.meshPositions mesh) (A.meshIndices mesh)]]
    assert ("CPU collision nodes disagree with renderer geometry: "++show (expected,hits)) (not (null hits) && maybe False (\distance->abs (minimum hits-distance)<0.001) expected)
  pass "CPU-only collider import agrees with renderer's transformed multi-node glTF geometry"

controllerReplay :: N.Window -> IO ()
controllerReplay window=do
  createDirectoryIfMissing True "test-output"
  let path="test-output/controller.rae"
  writeFile path "c 9\ne 0 9 0 0 0 0\ne 0 13 0 0 16384 0\ne 0 13 0 2 16384 0\ne 0 12 0 7 0 0\ne 1 9 0 0 0 0\ne 1 13 0 0 16384 0\ne 1 13 0 2 16384 0\ne 1 12 0 7 0 0\ne 2 10 0 0 0 0\n"
  original<-readIORef (N.uiState window)
  N.loadReplay window path >>= assert "Controller replay did not load"
  _<-N.frameTime window
  first<-N.input window
  firstFrame<-N.inputSnapshot window
  let (lookX,lookY)=I.lookDelta (I.actionFrame I.defaultBindings 1 I.emptyFrame firstFrame)
  assert "Native right stick did not reach orbital look" (lookX>0.7 && abs lookY<0.001)
  assert ("Native controller snapshot/mapping failed: "++show first) (abs (I.moveX first-(0.5-0.18)/0.82)<0.001 && I.jump first)
  focused<-readIORef (N.uiState window)
  writeIORef (N.uiState window) focused {N.focusedText=Just (42,"editing")}
  N.input window >>= assert "Native controller controls escaped text focus" . (==I.Input 0 0 False False False)
  focusedFrame<-N.inputSnapshot window
  let focusedActions=I.actionFrame I.defaultBindings 1 I.emptyFrame focusedFrame
  assert "Focused text leaked into frame actions" (I.gameInput False focusedFrame==I.Input 0 0 False False False && I.lookDelta focusedActions==(0,0) && not (I.actionPressed "jump" focusedActions))
  N.clearFocus window
  _<-N.frameTime window
  second<-N.input window
  assert "Held controller button was treated as one-frame press" (I.jump second)
  _<-N.frameTime window
  disconnected<-N.input window
  assert "Native disconnect retained controller movement/buttons" (disconnected==I.Input 0 0 False False False)
  disconnectedFrame<-N.inputSnapshot window
  assert "Native disconnect retained orbital look" (I.lookDelta (I.actionFrame I.defaultBindings 1 firstFrame disconnectedFrame)==(0,0))
  finished<-readIORef (N.uiState window)
  writeIORef (N.uiState window) original {N.replay=N.replay finished}
  pass "native replayed controller snapshot, analog mapping, held buttons, focus and disconnect"

animatedRendering :: N.Window -> R.Renderer -> (Float,Float,Float,Float) -> IO ()
animatedRendering window r rect=do
  let (_,base)=spawnMesh "Animated left" "test/fixtures/animation/fixture.gltf" emptyWorld
      (_,two)=spawnMesh "Animated right" "test/fixtures/animation/fixture.gltf" base
      (_,three)=spawnMesh "Shadow receiver" "cube" two
      pose time=defaultAnimator {animationClip="move",animationTime=time,animationLoop=False}
      scene=(emptyProject "Independent poses") {camera=Camera (V3 0 3 8) (V3 0 0.5 0) 45,environment=defaultEnvironment {sunShadows=True},world=three {animators=IM.fromList [(1,(Animation.crossfade 1 "single" (pose 0)) {animationBlend=Just (AnimationBlend "move" 0 0.4 1)}),(2,pose 1)],transforms=IM.fromList [(1,identity {position=V3 (-1.5) 1 0}),(2,identity {position=V3 1 1 0}),(3,identity {position=V3 0 (-0.5) 0,scale=V3 10 1 8})]}}
  render window scene rect False
  prior<-targetPixels r
  priorShadow<-shadowPixels
  cache<-readIORef (R.animatedInstances r)
  errors<-R.takeErrors r
  assert ("Animated import failed: "++show errors) (IM.member 1 cache && IM.member 2 cache)
  bounds<-readIORef (R.poseCache r)
  let (_,_,left)=cache IM.! 1;(_,_,right)=cache IM.! 2
      leftMesh=head (A.modelMeshes left);rightMesh=head (A.modelMeshes right)
  let palette mesh=maybe (error "Missing character palette") id (A.meshDeformation mesh)
  assert "Characters must share bind geometry and retain independent palettes" (G.vertexArray (A.meshBuffer leftMesh)==G.vertexArray (A.meshBuffer rightMesh) && G.paletteTexture (palette leftMesh)/=G.paletteTexture (palette rightMesh) && A.meshPositions leftMesh/=A.meshPositions rightMesh)
  bindBefore<-bufferBytes (G.vertexBuffer (A.meshBuffer leftMesh)) (VS.length (A.meshPositions leftMesh)*8*4)
  -- Ordinary rendering must not evaluate the on-demand CPU picking geometry.
  let poison model=model {A.modelMeshes=map (\mesh->mesh {A.meshPositions=error "Draw forced CPU skinning"}) (A.modelMeshes model)}
  writeIORef (R.animatedInstances r) (IM.map (\(path,config,model)->(path,config,poison model)) cache)
  render window scene rect False
  writeIORef (R.animatedInstances r) cache
  G.preservingState $ \_->do
    N.begin window
    R.flushUI
    glActiveTexture (GL_TEXTURE0+10)
    glBindTexture GL_TEXTURE_BUFFER (G.paletteTexture (palette rightMesh))
    glBindBuffer GL_TEXTURE_BUFFER (G.paletteBuffer (palette leftMesh))
    glActiveTexture (GL_TEXTURE0+4)
    before<-G.captureState
    _<-N.drawScene window "." scene rect False False Nothing
    after<-G.captureState
    assert ("Animated rendering changed borrowed texture-buffer bindings: "++show (before,after)) (before==after)
    N.end window
  -- Reference rendering disables shader deformation and uploads CPU poses once
  -- in this test, comparing both PBR pixels and the depth pass against the GPU.
  rig<-Animation.loadRig "test/fixtures/animation/fixture.gltf"
  cpuBuffers<-newIORef []
  reference<-IM.traverseWithKey (\e (path,config,model)->do
    meshes<-forM (zip (Animation.posedMeshes rig config) (A.modelMeshes model)) $ \(raw,mesh)->do
      buffer<-G.uploadBuffer raw (A.meshIndices mesh)
      modifyIORef' cpuBuffers (buffer:)
      pure mesh {A.meshBuffer=buffer,A.meshDeformation=Nothing}
    pure (path,config,model {A.modelMeshes=meshes})) cache
  writeIORef (R.animatedInstances r) reference
  render window scene rect False
  cpuPixels<-targetPixels r
  cpuShadow<-shadowPixels
  writeIORef (R.animatedInstances r) cache
  readIORef cpuBuffers >>= mapM_ G.deleteBuffer
  let pixelError=VS.sum (VS.zipWith (\a b->abs (fromIntegral a-fromIntegral b)::Int) prior cpuPixels)
  assert "GPU/CPU PBR deformation differs" (fromIntegral pixelError/fromIntegral (VS.length prior)<(0.5::Double))
  assert "GPU/CPU shadow deformation differs" (VS.maximum (VS.zipWith (\a b->abs (a-b)) priorShadow cpuShadow)<0.00001)
  let ray=C.Ray (L.V3 (-2) 1.1 5) (L.V3 0 0 (-1))
      draw=R.Draw 1 left (C.modelMatrix (transforms (world scene) IM.! 1) L.identity) defaultMaterial (R.whiteTexture r) False
  assert "Animated CPU picking lost triangle" (R.pick ray [draw] Nothing==Just 1)
  render window scene {world=(world scene) {animators=IM.adjust (\a->a {animationTime=1,animationBlend=Nothing}) 1 (animators (world scene))}} rect False
  later<-targetPixels r
  laterShadow<-shadowPixels
  assert "Animated vertices did not reach the shadow pass" (VS.sum (VS.zipWith (\x y->abs (x-y)) priorShadow laterShadow)>0.01)
  updated<-readIORef (R.poseCache r)
  bindAfter<-bufferBytes (G.vertexBuffer (A.meshBuffer leftMesh)) (VS.length (A.meshPositions leftMesh)*8*4)
  assert "Animation rewrote immutable bind-pose vertices" (bindBefore==bindAfter)
  assert "Changing only a pose retained stale bounds or GPU pixels" (R.cachedBounds (updated IM.! 1)/=R.cachedBounds (bounds IM.! 1) && VS.sum (VS.zipWith (\x y->abs (fromIntegral x-fromIntegral y)::Int) prior later)>1000)
  render window scene {world=(world scene) {animators=IM.adjust (\a->a {animationClip="typing-unknown"}) 1 (animators (world scene))}} rect False
  messages<-R.takeErrors r
  assert "Unknown clip did not remain editable with diagnostic" (not (null messages))
  render window scene {world=destroy 1 (world scene)} rect False
  glIsTexture (G.paletteTexture (palette leftMesh)) >>= assert "Deleted animation palette leaked" . (==GL_FALSE)
  glIsTexture (G.paletteTexture (palette rightMesh)) >>= assert "Deleting a character removed another palette" . (==GL_TRUE)
  glIsVertexArray (G.vertexArray (A.meshBuffer rightMesh)) >>= assert "Deleting one animation removed another" . (==GL_TRUE)
  N.unloadAssets window
  glIsVertexArray (G.vertexArray (A.meshBuffer rightMesh)) >>= assert "Animation unload leaked" . (==GL_FALSE)
  pass "GPU/CPU PBR and shadow parity, shared bind geometry, independent palettes, bounds/picking and instance lifetime"
  where
    bufferBytes buffer count=G.preservingState $ \_->do
      glBindBuffer GL_ARRAY_BUFFER buffer
      storage<-mallocForeignPtrArray count
      withForeignPtr storage $ \p->glGetBufferSubData GL_ARRAY_BUFFER 0 (fromIntegral count) p
      pure (VS.unsafeFromForeignPtr0 storage count :: VS.Vector Word8)
    shadowPixels=G.preservingState $ \_->do
      let t=R.shadowTarget r;count=G.targetWidth t*G.targetHeight t
      storage<-mallocForeignPtrArray count
      glBindFramebuffer GL_READ_FRAMEBUFFER (G.framebuffer t)
      withForeignPtr storage $ \p->glReadPixels 0 0 (fromIntegral (G.targetWidth t)) (fromIntegral (G.targetHeight t)) GL_DEPTH_COMPONENT GL_FLOAT p
      pure (VS.unsafeFromForeignPtr0 storage count :: VS.Vector Float)

terrainRendering :: N.Window -> R.Renderer -> (Float,Float,Float,Float) -> IO ()
terrainRendering window r rect=do
  let terrain=T.generateTerrain 17 17 0.5 (\x z->0.2*sin x*cos z)
      (e,w)=T.spawnTerrain "Finite GPU patch" terrain emptyWorld
      scene=(emptyProject "Terrain GPU") {world=w,camera=Camera (V3 6 5 8) (V3 0 0 0) 45,environment=defaultEnvironment {sunShadows=True}}
  render window scene rect False
  prior<-targetPixels r
  cache<-readIORef (R.terrainInstances r)
  bounds<-readIORef (R.poseCache r)
  let (_,original)=cache IM.! e;vao=G.vertexArray (A.meshBuffer (head (A.modelMeshes original)))
      edited=T.setHeight (8*17+8) 2 terrain
  render window scene {world=w {terrains=IM.singleton e edited}} rect False
  next<-targetPixels r
  poses<-readIORef (R.poseCache r)
  assert "Terrain edit retained old GPU pixels/bounds" (prior/=next && R.cachedBounds (bounds IM.! e)/=R.cachedBounds (poses IM.! e))
  glIsVertexArray vao >>= assert "Terrain replacement leaked previous buffers" . (==GL_FALSE)
  _<-R.getTerrainModel r e edited
  current<-readIORef (R.terrainInstances r)
  let (_,model)=current IM.! e;mesh=head (A.modelMeshes model)
      ray=C.Ray (L.V3 0 5 0) (L.V3 0 (-1) 0)
  assert "Edited terrain CPU picking missed center height" (maybe False (\distance->abs (distance-3)<0.001) (C.rayMesh ray (A.meshBounds mesh) (A.meshPositions mesh) (A.meshIndices mesh)))
  render window scene {world=destroy e w} rect False
  glIsVertexArray (G.vertexArray (A.meshBuffer mesh)) >>= assert "Terrain deletion leaked buffers" . (==GL_FALSE)
  N.unloadAssets window
  pass "finite terrain edited pixels/bounds/picking and replacement/deletion lifetime"
