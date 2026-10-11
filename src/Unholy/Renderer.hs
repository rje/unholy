{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
-- Haskell owns scene preparation, asset caches, GPU lifetime, and every 3D draw.
module Unholy.Renderer where

import Unholy.Types
import Unholy.Transform (setTransform,rotationQ)
import qualified Unholy.Gizmo as Gizmo
import Unholy.Project (builtins)
import Unholy.Renderer.GL
import Unholy.Renderer.Camera
import Unholy.Renderer.Assets
import qualified Unholy.Renderer.GLTF as F
import qualified Unholy.Renderer.IBL as IBL
import Graphics.GL
import qualified Linear as L
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Data.IORef
import Foreign
import Control.Exception (IOException, bracketOnError, onException, try)
import Control.Monad (forM, forM_, when, unless)
import System.FilePath ((</>))

foreign import ccall unsafe "rlDrawRenderBatchActive" flushUI :: IO ()

data Renderer = Renderer
  { pbrProgram :: !Program, shadowProgram :: !Program, screenProgram :: !Program, lineProgram :: !Program, iblProgram :: !Program
  , shadowTarget :: !Target, viewportTarget :: !(IORef (Maybe Target))
  , meshCache :: !(IORef (M.Map FilePath ModelData)), textureCache :: !(IORef (M.Map FilePath GLuint))
  , whiteTexture :: !GLuint, neutralMaps :: ![GLuint], screenBuffer :: !Buffer, gridBuffer :: !Buffer, boxBuffer :: !Buffer
  , axisBuffers :: ![Buffer], lightBuffer :: !Buffer, capsuleBuffer :: !Buffer
  , orbitState :: !(IORef Orbit), activeCamera :: !(IORef Camera), rendererErrors :: !(IORef [String])
  , gizmoMode :: !(IORef (Maybe Gizmo.Mode)), gizmoDrag :: !(IORef (Maybe Gizmo.Drag)), gizmoEdit :: !(IORef (Maybe Gizmo.Edit)), ringBuffers :: ![Buffer]
  , instanceBuffer :: !GLuint, poseCache :: !(IORef (IM.IntMap CachedPose))
  , instancingEnabled :: !(IORef Bool), cullingEnabled :: !(IORef Bool), frameStats :: !(IORef FrameStats)
  , environmentCache :: !(IORef (Maybe (FilePath,Maybe IBL.IBL)))
  , disposed :: !(IORef Bool)
  }
data Draw = Draw { drawEntity :: !Entity, drawModel :: !ModelData, drawMatrix :: !(L.M44 Float), drawMaterial :: !Material, drawTexture :: !GLuint, drawImported :: !Bool }
data CachedPose = CachedPose { cachedGeometry :: !FilePath, cachedModel :: !GLuint, cachedTransform :: !Transform, cachedMatrix :: !(L.M44 Float), cachedNormal :: !(L.M44 Float), cachedBounds :: !Bounds, cachedColumns :: ![L.M44 Float] }
data FrameStats = FrameStats { totalMeshes :: !Int, visibleMeshes :: !Int, colorCalls :: !Int, shadowCalls :: !Int } deriving (Eq,Show)
data Surface = Surface !Bool !(L.V3 Float) !Float !Float !Float !(L.V3 Float) !Float !Float ![GLuint] deriving (Eq,Ord)
type Prepared = (Draw,CachedPose)

initialize :: FilePath -> IO Renderer
initialize root = preservingState $ \_ ->
  bracketOnError (load "pbr") (glDeleteProgram . programId) $ \pbrProgram ->
  bracketOnError (load "depth") (glDeleteProgram . programId) $ \shadowProgram ->
  bracketOnError (load "screen") (glDeleteProgram . programId) $ \screenProgram ->
  bracketOnError (load "line") (glDeleteProgram . programId) $ \lineProgram ->
  bracketOnError (loadProgram (root </> "shaders/screen.vs") (root </> "shaders/ibl.fs")) (glDeleteProgram . programId) $ \iblProgram ->
  bracketOnError (newTarget True 2048 2048) deleteTarget $ \shadowTarget ->
  bracketOnError (withArray ([255,255,255,255]::[Word8]) (newTexture 1 1)) (deleteObject glDeleteTextures) $ \whiteTexture ->
  bracketOnError (lineBuffer [L.V3 (-1) (-1) 0,L.V3 3 (-1) 0,L.V3 (-1) 3 0]) deleteBuffer $ \screenBuffer ->
  bracketOnError (lineBuffer gridVertices) deleteBuffer $ \gridBuffer ->
  bracketOnError (lineBuffer boxVertices) deleteBuffer $ \boxBuffer ->
  bracketOnError (loadBuffers [[L.V3 0 0 0,p] | p<-[L.V3 1 0 0,L.V3 0 1 0,L.V3 0 0 1]]) (mapM_ deleteBuffer) $ \axisBuffers ->
  bracketOnError (lineBuffer lightVertices) deleteBuffer $ \lightBuffer ->
  bracketOnError (lineBuffer capsuleVertices) deleteBuffer $ \capsuleBuffer ->
  bracketOnError (loadBuffers [concat (zipWith (\a b->[a,b]) points (drop 1 points)) | axis<-[0..2],let points=Gizmo.ringPoints axis]) (mapM_ deleteBuffer) $ \ringBuffers ->
  bracketOnError (genObject glGenBuffers) (deleteObject glDeleteBuffers) $ \instanceBuffer -> do
    gizmoMode<-newIORef Nothing
    gizmoDrag<-newIORef Nothing
    gizmoEdit<-newIORef Nothing
    viewportTarget<-newIORef Nothing
    meshCache<-newIORef M.empty
    textureCache<-newIORef M.empty
    orbitState<-newIORef defaultOrbit
    activeCamera<-newIORef defaultCamera
    rendererErrors<-newIORef []
    poseCache<-newIORef IM.empty
    instancingEnabled<-newIORef True
    cullingEnabled<-newIORef True
    frameStats<-newIORef (FrameStats 0 0 0 0)
    let neutralMaps=replicate 6 whiteTexture
    environmentCache<-newIORef Nothing
    disposed<-newIORef False
    checkGL "Haskell renderer initialization"
    putStrLn "Renderer: Haskell / OpenGL 3.3 (direct GPU submission)"
    pure Renderer{..}
  where load name=loadProgram (root </> "shaders" </> name++".vs") (root </> "shaders" </> name++".fs")

loadBuffers :: [[L.V3 Float]] -> IO [Buffer]
loadBuffers []=pure []
loadBuffers (vs:rest)=do
  b<-lineBuffer vs
  bs<-loadBuffers rest `onException` deleteBuffer b
  pure (b:bs)
lineBuffer :: [L.V3 Float] -> IO Buffer
lineBuffer vertices = uploadBuffer (VS.fromList (concatMap (\(L.V3 x y z)->[x,y,z,0,0,0,1,0]) vertices)) VS.empty
gridVertices :: [L.V3 Float]
gridVertices = concat [[L.V3 n 0 (-20),L.V3 n 0 20,L.V3 (-20) 0 n,L.V3 20 0 n] | n<-[-20..20]]
boxVertices :: [L.V3 Float]
boxVertices = concat [[a,b] | a<-corners,b<-corners,a<b,length (filter id (zipWith (/=) (toList a) (toList b)))==1]
  where corners=boundsCorners (L.V3 (-0.5) (-0.5) (-0.5),L.V3 0.5 0.5 0.5)
        toList (L.V3 x y z)=[x,y,z]
lightVertices :: [L.V3 Float]
lightVertices = concat [[point axis t,point axis (t+step)] | axis<-[0..2::Int],i<-[0..31::Int],let t=fromIntegral i*step]
  where step=2*pi/32
        point 0 t=L.V3 0 (cos t) (sin t)
        point 1 t=L.V3 (cos t) 0 (sin t)
        point _ t=L.V3 (cos t) (sin t) 0

capsuleVertices :: [L.V3 Float]
capsuleVertices=concat [concat (zipWith (\a b->[a,b]) points (drop 1 points)) | plane<-[False,True],let points=outline plane]
  where outline plane=map (point plane) ([(cos a/2,0.5+sin a/2) | i<-[0..32::Int],let a=pi*fromIntegral i/32]++[(cos a/2,-0.5+sin a/2) | i<-[32..64::Int],let a=pi*fromIntegral i/32]++[(0.5,0.5)])
        point False (x,y)=L.V3 x y 0
        point True (z,y)=L.V3 0 y z

unloadAssets :: Renderer -> IO ()
unloadAssets Renderer{..} = do
  flushUI
  models<-atomicModifyIORef' meshCache (\m -> (M.empty,m))
  textures<-atomicModifyIORef' textureCache (\m -> (M.empty,m))
  priorEnvironment<-atomicModifyIORef' environmentCache (\value->(Nothing,value))
  mapM_ (mapM_ IBL.deleteIBL . snd) priorEnvironment
  writeIORef poseCache IM.empty
  mapM_ deleteModel (M.elems models)
  mapM_ (deleteObject glDeleteTextures) (filter (/=whiteTexture) (M.elems textures))
close :: Renderer -> IO ()
close r@Renderer{..} = do
  alreadyClosed<-atomicModifyIORef' disposed (\closed -> (True,closed))
  unless alreadyClosed $ do
    unloadAssets r
    readIORef viewportTarget >>= mapM_ deleteTarget
    writeIORef viewportTarget Nothing
    deleteTarget shadowTarget
    mapM_ (glDeleteProgram . programId) [pbrProgram,shadowProgram,screenProgram,lineProgram,iblProgram]
    mapM_ deleteBuffer ([screenBuffer,gridBuffer,boxBuffer,lightBuffer,capsuleBuffer]++axisBuffers++ringBuffers)
    deleteObject glDeleteTextures whiteTexture
    deleteObject glDeleteBuffers instanceBuffer
focus :: Renderer -> V3 -> IO ()
focus r pos = modifyIORef' (orbitState r) (\o -> o {orbitTarget=toLinear pos,orbitDistance=7})
getCamera :: Renderer -> IO Camera
getCamera = readIORef . activeCamera
takeErrors :: Renderer -> IO [String]
takeErrors r = atomicModifyIORef' (rendererErrors r) (\errors -> ([],reverse errors))
reportError :: Renderer -> IOException -> IO ()
reportError r err = modifyIORef' (rendererErrors r) (show err:)

getModel :: Renderer -> FilePath -> IO ModelData
getModel r=getModelFor r True
getModelFor :: Renderer -> Bool -> FilePath -> IO ModelData
getModelFor r importedPolicy path = do
  cached<-M.lookup path <$> readIORef (meshCache r)
  case cached of
    Just model->pure model
    Nothing->do
      imported<-try (loadModel path)
      model<-case imported of
        Right value->pure value
        Left err->reportError r err >> loadModel "cube"
      when importedPolicy (forM_ (materialError model) (reportError r . userError))
      modifyIORef' (meshCache r) (M.insert path model)
      pure model
getEnvironment :: Renderer -> FilePath -> IO (Maybe IBL.IBL)
getEnvironment r path = do
  cached<-readIORef (environmentCache r)
  case cached of
    Just (old,maps) | old==path->pure maps
    _->do
      loaded<-if null path then pure Nothing else do
        attempt<-try (IBL.loadIBL (iblProgram r) (screenBuffer r) path)
        case attempt of Right maps->pure (Just maps);Left err->reportError r err >> pure Nothing
      writeIORef (environmentCache r) (Just (path,loaded))
      mapM_ (mapM_ IBL.deleteIBL . snd) cached
      pure loaded

setIBL :: Program -> Environment -> Maybe IBL.IBL -> IO ()
setIBL shader env maps = do
  intUniform shader "iblEnabled" (maybe 0 (const 1) maps)
  floatUniform shader "environmentIntensity" (environmentIntensity env)
  floatUniform shader "environmentRotation" (environmentRotation env*pi/180)
  floatUniform shader "prefilterLevels" 6
  forM_ [("irradianceMap",7),("prefilterMap",8),("brdfLUT",9)] $ \(name,unit)->intUniform shader name unit

getTexture :: Renderer -> FilePath -> IO GLuint
getTexture r "" = pure (whiteTexture r)
getTexture r path = do
  cached<-M.lookup path <$> readIORef (textureCache r)
  case cached of
    Just tex->pure tex
    Nothing->do
      loaded<-try (loadTexture path)
      tex<-case loaded of Right value->pure value;Left err->reportError r err >> pure (whiteTexture r)
      modifyIORef' (textureCache r) (M.insert path tex)
      pure tex

ensureViewport :: Renderer -> Int -> Int -> IO Target
ensureViewport r w h = do
  current<-readIORef (viewportTarget r)
  case current of
    Just t | targetWidth t==w && targetHeight t==h -> pure t
    _->do
      t<-newTarget False w h
      writeIORef (viewportTarget r) (Just t)
      mapM_ deleteTarget current
      pure t
drawScene :: Renderer -> Mouse -> Bool -> Bool -> FilePath -> Project -> (Float,Float,Float,Float) -> (Float,Float) -> Bool -> Bool -> Maybe Entity -> IO (Maybe Entity)
drawScene r mouse down snapping root project rect@(x,y,w,h) (screenW,screenH) edit grid selected = do
  closed<-readIORef (disposed r)
  when closed (ioError (userError "Cannot draw with a disposed renderer"))
  flushUI
  preservingState $ \saved -> do
    orbit<-readIORef (orbitState r)
    let inputRect=(x,y+45,w,max 1 (h-80))
        sceneMouse=mouse {mouseEnabled=mouseEnabled mouse && inside inputRect mouse}
        nextOrbit=if edit then updateOrbit inputRect sceneMouse orbit else orbit
        cam=if edit then orbitCamera nextOrbit else camera project
        env=environment project
        L.V4 vx vy screenPixelsW screenPixelsH=savedViewport saved
        ratioX=fromIntegral screenPixelsW/max 1 screenW
        ratioY=fromIntegral screenPixelsH/max 1 screenH
        renderW=max 1 (round (w*ratioX));renderH=max 1 (round (h*ratioY))
        vp=viewProjection (fromIntegral renderW/fromIntegral renderH) cam
        lightVP=sunMatrix cam env
    writeIORef (orbitState r) nextOrbit
    writeIORef (activeCamera r) cam
    mode<-readIORef (gizmoMode r)
    drag<-readIORef (gizmoDrag r)
    let active=if edit then mode else Nothing
        selection=selected >>= (\e->(,) e <$> IM.lookup e (transforms (world project)))
        (nextDrag,edited,captured)=Gizmo.update rect cam sceneMouse down snapping active selection drag
        displayed=maybe (world project) (\change->setTransform (Gizmo.editEntity change) (Gizmo.editTransform change) (world project)) edited
    writeIORef (gizmoDrag r) nextDrag
    writeIORef (gizmoEdit r) edited
    priorPoses<-readIORef (poseCache r)
    prepared<-forM (renderables displayed) $ \(e,t,m,mat) -> do
      let path=if geometry m `elem` builtins then geometry m else root </> geometry m
      model<-getModelFor r (useImportedMaterials m) path
      tex<-getTexture r (if null (texture m) then "" else root </> texture m)
      let pose=case IM.lookup e priorPoses of
            Just cached | cachedGeometry cached==path && cachedModel cached==vertexArray (meshBuffer (head (modelMeshes model))) && cachedTransform cached==t -> cached
            _->let matrix=modelMatrix t (importTransform model);normal=L.transpose (L.inv44 matrix)
               in CachedPose path (vertexArray (meshBuffer (head (modelMeshes model)))) t matrix normal (transformBounds matrix (modelBounds model)) [L.transpose matrix,L.transpose normal]
      pure (Draw e model (cachedMatrix pose) mat tex (useImportedMaterials m),pose)
    writeIORef (poseCache r) (IM.fromDistinctAscList [(drawEntity d,pose) | (d,pose)<-prepared])
    useBatch<-readIORef (instancingEnabled r)
    cull<-readIORef (cullingEnabled r)
    let draws=map fst prepared
        visible=if cull then filter (inFrustum (frustumPlanes vp) . cachedBounds . snd) prepared else prepared
        colors=M.fromListWith merge [( (vertexArray buffer,surface), (buffer,surface,[pose])) | (d,pose)<-visible,mesh<-modelMeshes (drawModel d),let buffer=meshBuffer mesh;surface=resolveSurface r d mesh]
        shadows=M.fromListWith mergeShadow [(vertexArray buffer,(buffer,[pose])) | (d,pose)<-prepared,mesh<-modelMeshes (drawModel d),let buffer=meshBuffer mesh]
        callCount poses=if useBatch && length poses>1 then 1 else length poses
    writeIORef (frameStats r) (FrameStats (sum [length (modelMeshes (drawModel d)) | (d,_)<-prepared]) (sum [length (modelMeshes (drawModel d)) | (d,_)<-visible]) (sum [callCount poses | (_,_,poses)<-M.elems colors]) (if sunShadows env then sum [callCount poses | (_,poses)<-M.elems shadows] else 0))
    ibl<-getEnvironment r (if null (environmentMap env) then "" else root </> environmentMap env)
    viewport<-ensureViewport r renderW renderH
    glDisable GL_SCISSOR_TEST
    glDisable GL_BLEND
    glEnable GL_CULL_FACE
    glCullFace GL_BACK
    glEnable GL_DEPTH_TEST
    glDepthFunc GL_LEQUAL
    glDepthMask GL_TRUE
    when (sunShadows env) $ do
      bindTarget (shadowTarget r)
      glClear GL_DEPTH_BUFFER_BIT
      glUseProgram (programId (shadowProgram r))
      matrixUniform (shadowProgram r) "viewProjection" lightVP
      forM_ (M.elems shadows) $ \(buffer,poses)->submit r useBatch (shadowProgram r) lightVP buffer poses
    bindTarget viewport
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    glDisable GL_DEPTH_TEST
    glDisable GL_CULL_FACE
    glUseProgram (programId (screenProgram r))
    intUniform (screenProgram r) "mode" 0
    intUniform (screenProgram r) "texture0" 0
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D (whiteTexture r)
    setIBL (screenProgram r) env ibl
    intUniform (screenProgram r) "environmentMap" 7
    matrixUniform (screenProgram r) "inverseVP" (L.inv44 vp)
    vectorUniform (screenProgram r) "viewPos" (toLinear (eye cam))
    floatUniform (screenProgram r) "exposure" (exposure env)
    forM_ ibl $ \maps->glActiveTexture (GL_TEXTURE0+7) >> glBindTexture GL_TEXTURE_2D (IBL.radianceTexture maps)
    vectorUniform (screenProgram r) "skyTop" (toLinear (skyTop env))
    vectorUniform (screenProgram r) "skyBottom" (toLinear (skyBottom env))
    drawBuffer GL_TRIANGLES (screenBuffer r)
    glEnable GL_DEPTH_TEST
    glEnable GL_CULL_FACE
    setEnvironment r cam env lightVP displayed
    setIBL (pbrProgram r) env ibl
    glEnable GL_TEXTURE_CUBE_MAP_SEAMLESS
    forM_ ibl $ \maps->do
      forM_ [(7,IBL.irradianceTexture maps),(8,IBL.prefilterTexture maps)] $ \(unit,tex)->glActiveTexture (GL_TEXTURE0+unit) >> glBindTexture GL_TEXTURE_CUBE_MAP tex
      glActiveTexture (GL_TEXTURE0+9)
      glBindTexture GL_TEXTURE_2D (IBL.brdfTexture maps)
    matrixUniform (pbrProgram r) "viewProjection" vp
    forM_ (M.elems colors) $ \(buffer,surface,poses)->do
      applySurface (pbrProgram r) (whiteTexture r) surface
      submit r useBatch (pbrProgram r) vp buffer poses
    when edit $ do
      drawOverlays r vp displayed draws grid selected
      forM_ active $ \tool -> forM_ (selected >>= (\e->IM.lookup e (transforms displayed))) $ \pose ->
        drawGizmo r vp rect cam tool pose (Gizmo.dragAxis <$> nextDrag)
    -- Composite directly into the window's original framebuffer, using its
    -- pixel viewport to keep logical editor coordinates independent of DPI.
    glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral (savedDrawFbo saved))
    glViewport (vx+round (x*ratioX)) (vy+screenPixelsH-round ((y+h)*ratioY)) (fromIntegral renderW) (fromIntegral renderH)
    glDisable GL_DEPTH_TEST
    glDisable GL_CULL_FACE
    glUseProgram (programId (screenProgram r))
    intUniform (screenProgram r) "mode" 1
    intUniform (screenProgram r) "texture0" 0
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D (colorTexture viewport)
    drawBuffer GL_TRIANGLES (screenBuffer r)
    checkGL "Haskell scene rendering"
    pure $ if edit && not captured && mouseEnabled sceneMouse && leftPressed mouse && inside inputRect mouse then pick (screenRay rect cam mouse) draws selected else selected

merge :: (Buffer,Surface,[CachedPose]) -> (Buffer,Surface,[CachedPose]) -> (Buffer,Surface,[CachedPose])
merge (buffer,surface,a) (_,_,b)=(buffer,surface,a++b)
mergeShadow :: (Buffer,[CachedPose]) -> (Buffer,[CachedPose]) -> (Buffer,[CachedPose])
mergeShadow (buffer,a) (_,b)=(buffer,a++b)
submit :: Renderer -> Bool -> Program -> L.M44 Float -> Buffer -> [CachedPose] -> IO ()
submit r batches shader vp buffer poses
  | batches && length poses>1 = do
      intUniform shader "useInstances" 1
      drawInstances (instanceBuffer r) buffer (VS.fromList (concatMap cachedColumns poses))
  | otherwise = do
      intUniform shader "useInstances" 0
      forM_ poses $ \pose->do
        matrixUniform shader "mvp" (vp L.!*! cachedMatrix pose)
        matrixUniform shader "matModel" (cachedMatrix pose)
        matrixUniform shader "matNormal" (cachedNormal pose)
        drawBuffer GL_TRIANGLES buffer
resolveSurface :: Renderer -> Draw -> MeshData -> Surface
resolveSurface r d mesh = Surface (maybe False (const True) source) color metal rough emit glow (F.normalScale info) occlusion maps
  where
    mat=drawMaterial d
    source=if drawImported d then importedMaterial mesh else Nothing
    info=maybe F.defaultInfo materialInfo source
    color=maybe (toLinear (albedo mat)) (F.baseFactor . materialInfo) source
    metal=maybe (metallic mat) (F.metalFactor . materialInfo) source
    rough=maybe (roughness mat) (F.roughFactor . materialInfo) source
    emit=maybe (emission mat) (const 0) source
    glow=maybe (L.V3 0 0 0) (F.emissiveFactor . materialInfo) source
    occlusion=maybe 0 (F.occlusionStrength . materialInfo) source
    base=if drawTexture d==whiteTexture r then maybe (whiteTexture r) id (importedTexture mesh) else drawTexture d
    maps=case source of
      Nothing | base==whiteTexture r -> neutralMaps r
              | otherwise -> base:drop 1 (neutralMaps r)
      Just imported->base:[maybe (whiteTexture r) id (textureSlot i imported) | i<-[1..5]]
applySurface :: Program -> GLuint -> Surface -> IO ()
applySurface shader white (Surface linear color metal rough emit glow normal occlusion maps) = do
  intUniform shader "baseColorLinear" (if linear then 1 else 0)
  vectorUniform shader "baseColor" color
  vectorUniform shader "emissiveColor" glow
  forM_ [("metalness",metal),("roughness",rough),("emission",emit),("normalScale",normal),("occlusionStrength",occlusion)] $ \(name,value)->floatUniform shader name value
  intUniform shader "useNormalMap" (if maps!!1/=white then 1 else 0)
  forM_ (zip [0,2,3,4,5,6] maps) $ \(unit,tex)->glActiveTexture (GL_TEXTURE0+unit) >> glBindTexture GL_TEXTURE_2D tex

drawGeometry :: Draw -> IO ()
drawGeometry d = forM_ (modelMeshes (drawModel d)) (drawBuffer GL_TRIANGLES . meshBuffer)
setEnvironment :: Renderer -> Camera -> Environment -> L.M44 Float -> World -> IO ()
setEnvironment r cam env lightVP w = do
  let shader=pbrProgram r
      ls=take 8 [(position t,l) | (e,l)<-IM.toAscList (lights w),Just t<-[IM.lookup e (transforms w)]]
  glUseProgram (programId shader)
  forM_ [("viewPos",eye cam),("sunDirection",sunDirection env),("sunColor",sunColor env),("fogColor",skyBottom env)] $ \(n,v) -> vectorUniform shader n (toLinear v)
  forM_ [("sunPower",sunPower env),("ambientPower",ambient env),("exposure",exposure env),("fogDensity",fogDensity env)] $ \(n,v) -> floatUniform shader n v
  intUniform shader "useShadows" (if sunShadows env then 1 else 0)
  matrixUniform shader "lightVP" lightVP
  intUniform shader "texture0" 0
  intUniform shader "shadowMap" 1
  forM_ (zip ["normalMap","metalMap","roughMap","emissiveMap","occlusionMap"] [2..6]) $ \(name,unit)->intUniform shader name unit
  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D (depthTexture (shadowTarget r))
  intUniform shader "lightCount" (length ls)
  forM_ (zip [0..] ls) $ \(i,(pos,l)) -> do
    let suffix="["++show (i::Int)++"]"
    vectorUniform shader ("lightPosition"++suffix) (toLinear pos)
    vectorUniform shader ("lightColor"++suffix) (toLinear (lightColor l))
    floatUniform shader ("lightPower"++suffix) (intensity l)

drawOverlays :: Renderer -> L.M44 Float -> World -> [Draw] -> Bool -> Maybe Entity -> IO ()
drawOverlays r vp w draws grid selected = do
  let shader=lineProgram r
      line color matrix buffer = do
        vectorUniform shader "baseColor" color
        matrixUniform shader "mvp" (vp L.!*! matrix)
        drawBuffer GL_LINES buffer
      accent=L.V3 (98/255) (161/255) (246/255)
      trs pos size=L.mkTransformation (L.Quaternion 1 (L.V3 0 0 0)) pos L.!*! L.scaled (let L.V3 a b c=size in L.V4 a b c 1)
  glUseProgram (programId shader)
  when grid (line (L.V3 0.35 0.35 0.35) L.identity (gridBuffer r))
  forM_ draws $ \d -> when (Just (drawEntity d)==selected) $ do
    let (lo,hi)=transformBounds (drawMatrix d) (modelBounds (drawModel d))
    line accent (trs ((lo+hi)/2) (hi-lo)) (boxBuffer r)
  forM_ (selected >>= (\e->(,) e <$> IM.lookup e (bodies w))) $ \(e,body)->forM_ (IM.lookup e (transforms w)) $ \pose->do
    let color=if trigger body then L.V3 1 0.7 0.2 else accent
        size=if bodyShape body==Sphere then let radius=maximum (vlist (scale pose))/2 in L.V3 radius radius radius else toLinear (scale pose)
        matrix=L.mkTransformation (rotationQ (rotation pose)) (toLinear (position pose)) L.!*! L.scaled (let L.V3 a b c=size in L.V4 a b c 1)
    case bodyShape body of
      Sphere->line color matrix (lightBuffer r)
      Capsule->line color matrix (capsuleBuffer r)
      Box->line color matrix (boxBuffer r)
      _->pure () -- The selected mesh bounds above show imported collider extent.
  forM_ (IM.toAscList (lights w)) $ \(e,_) -> forM_ (IM.lookup e (transforms w)) $ \t -> do
    let radius=if Just e==selected then 0.2 else 0.12
    line (if Just e==selected then accent else L.V3 1 0.84 0.52) (trs (toLinear (position t)) (L.V3 radius radius radius)) (lightBuffer r)

pick :: Ray -> [Draw] -> Maybe Entity -> Maybe Entity
pick ray draws selected = maybe selected (Just . snd) nearest
  where
    hits=[(distance,drawEntity d) | d<-draws,let localRay=objectRay (drawMatrix d) ray,mesh<-modelMeshes (drawModel d),Just distance<-[rayMesh localRay (meshBounds mesh) (meshPositions mesh) (meshIndices mesh)]]
    nearest=case hits of []->Nothing;_->Just (minimum hits)

setGizmoMode :: Renderer -> Maybe Gizmo.Mode -> IO ()
setGizmoMode r=writeIORef (gizmoMode r)
takeGizmoEdit :: Renderer -> IO (Maybe Gizmo.Edit)
takeGizmoEdit r=atomicModifyIORef' (gizmoEdit r) (\value->(Nothing,value))
drawGizmo :: Renderer -> L.M44 Float -> (Float,Float,Float,Float) -> Camera -> Gizmo.Mode -> Transform -> Maybe Int -> IO ()
drawGizmo r vp rect cam mode pose active = do
  let shader=lineProgram r;size=Gizmo.handleSize rect cam pose
      q=if mode==Gizmo.Scale then rotationQ (rotation pose) else L.Quaternion 1 (L.V3 0 0 0)
      matrix=L.mkTransformation q (toLinear (position pose)) L.!*! L.scaled (L.V4 size size size 1)
      colors=[L.V3 1 0.22 0.22,L.V3 0.2 1 0.4,L.V3 0.25 0.55 1]
      buffers=if mode==Gizmo.Rotate then ringBuffers r else axisBuffers r
  glDisable GL_DEPTH_TEST
  glUseProgram (programId shader)
  forM_ (zip3 [0..] buffers colors) $ \(i,buffer,color)->do
    vectorUniform shader "baseColor" (if active==Just i then L.V3 1 0.85 0.25 else color)
    matrixUniform shader "mvp" (vp L.!*! matrix)
    drawBuffer GL_LINES buffer
    unless (mode==Gizmo.Rotate) $ do
      let endpoint=toLinear (position pose)+size L.*^Gizmo.axisVector mode pose i
          handle=L.mkTransformation q endpoint L.!*! L.scaled (L.V4 (size*0.08) (size*0.08) (size*0.08) 1)
      matrixUniform shader "mvp" (vp L.!*! handle)
      drawBuffer GL_LINES (boxBuffer r)
