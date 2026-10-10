{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
-- Haskell owns scene preparation, asset caches, GPU lifetime, and every 3D draw.
module HJ.Renderer where

import HJ.Types
import HJ.Project (builtins)
import HJ.Renderer.GL
import HJ.Renderer.Camera
import HJ.Renderer.Assets
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
foreign import ccall unsafe "hj_render_input" cMouse :: Ptr Float -> IO ()

data Renderer = Renderer
  { pbrProgram :: !Program, shadowProgram :: !Program, screenProgram :: !Program, lineProgram :: !Program
  , shadowTarget :: !Target, viewportTarget :: !(IORef (Maybe Target))
  , meshCache :: !(IORef (M.Map FilePath ModelData)), textureCache :: !(IORef (M.Map FilePath GLuint))
  , whiteTexture :: !GLuint, screenBuffer :: !Buffer, gridBuffer :: !Buffer, boxBuffer :: !Buffer
  , axisBuffers :: ![Buffer], lightBuffer :: !Buffer
  , orbitState :: !(IORef Orbit), activeCamera :: !(IORef Camera), rendererErrors :: !(IORef [String])
  , disposed :: !(IORef Bool)
  }
data Draw = Draw { drawEntity :: !Entity, drawModel :: !ModelData, drawMatrix :: !(L.M44 Float), drawMaterial :: !Material, drawTexture :: !GLuint }

initialize :: FilePath -> IO Renderer
initialize root = preservingState $ \_ ->
  bracketOnError (load "pbr") (glDeleteProgram . programId) $ \pbrProgram ->
  bracketOnError (load "depth") (glDeleteProgram . programId) $ \shadowProgram ->
  bracketOnError (load "screen") (glDeleteProgram . programId) $ \screenProgram ->
  bracketOnError (load "line") (glDeleteProgram . programId) $ \lineProgram ->
  bracketOnError (newTarget True 2048 2048) deleteTarget $ \shadowTarget ->
  bracketOnError (withArray ([255,255,255,255]::[Word8]) (newTexture 1 1)) (deleteObject glDeleteTextures) $ \whiteTexture ->
  bracketOnError (lineBuffer [L.V3 (-1) (-1) 0,L.V3 3 (-1) 0,L.V3 (-1) 3 0]) deleteBuffer $ \screenBuffer ->
  bracketOnError (lineBuffer gridVertices) deleteBuffer $ \gridBuffer ->
  bracketOnError (lineBuffer boxVertices) deleteBuffer $ \boxBuffer ->
  bracketOnError (loadBuffers [[L.V3 0 0 0,p] | p<-[L.V3 1.5 0 0,L.V3 0 1.5 0,L.V3 0 0 1.5]]) (mapM_ deleteBuffer) $ \axisBuffers ->
  bracketOnError (lineBuffer lightVertices) deleteBuffer $ \lightBuffer -> do
    viewportTarget<-newIORef Nothing
    meshCache<-newIORef M.empty
    textureCache<-newIORef M.empty
    orbitState<-newIORef defaultOrbit
    activeCamera<-newIORef defaultCamera
    rendererErrors<-newIORef []
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

unloadAssets :: Renderer -> IO ()
unloadAssets Renderer{..} = do
  flushUI
  models<-atomicModifyIORef' meshCache (\m -> (M.empty,m))
  textures<-atomicModifyIORef' textureCache (\m -> (M.empty,m))
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
    mapM_ (glDeleteProgram . programId) [pbrProgram,shadowProgram,screenProgram,lineProgram]
    mapM_ deleteBuffer ([screenBuffer,gridBuffer,boxBuffer,lightBuffer]++axisBuffers)
    deleteObject glDeleteTextures whiteTexture
focus :: Renderer -> V3 -> IO ()
focus r pos = modifyIORef' (orbitState r) (\o -> o {orbitTarget=toLinear pos,orbitDistance=7})
getCamera :: Renderer -> IO Camera
getCamera = readIORef . activeCamera
takeErrors :: Renderer -> IO [String]
takeErrors r = atomicModifyIORef' (rendererErrors r) (\errors -> ([],reverse errors))
reportError :: Renderer -> IOException -> IO ()
reportError r err = modifyIORef' (rendererErrors r) (show err:)

getModel :: Renderer -> FilePath -> IO ModelData
getModel r path = do
  cached<-M.lookup path <$> readIORef (meshCache r)
  case cached of
    Just model->pure model
    Nothing->do
      imported<-try (loadModel path)
      model<-case imported of
        Right value->pure value
        Left err->reportError r err >> loadModel "cube"
      modifyIORef' (meshCache r) (M.insert path model)
      pure model
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
getMouse :: IO Mouse
getMouse = allocaArray 9 $ \p -> do
  cMouse p
  values<-peekArray 9 p
  case values of
    [x,y,dx,dy,wheel,right,middle,left,enabled]->pure (Mouse x y dx dy wheel (right/=0) (middle/=0) (left/=0) (enabled/=0))
    _->ioError (userError "Invalid platform mouse snapshot")

drawScene :: Renderer -> FilePath -> Project -> (Float,Float,Float,Float) -> (Float,Float) -> Bool -> Bool -> Maybe Entity -> IO (Maybe Entity)
drawScene r root project rect@(x,y,w,h) (screenW,screenH) edit grid selected = do
  closed<-readIORef (disposed r)
  when closed (ioError (userError "Cannot draw with a disposed renderer"))
  flushUI
  preservingState $ \saved -> do
    mouse<-getMouse
    orbit<-readIORef (orbitState r)
    let nextOrbit=if edit then updateOrbit rect mouse orbit else orbit
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
    draws<-forM (renderables (world project)) $ \(e,t,m,mat) -> do
      model<-getModel r (if geometry m `elem` builtins then geometry m else root </> geometry m)
      tex<-getTexture r (if null (texture m) then "" else root </> texture m)
      pure (Draw e model (modelMatrix t (importTransform model)) mat tex)
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
      forM_ draws $ \d -> do
        matrixUniform (shadowProgram r) "mvp" (lightVP L.!*! drawMatrix d)
        drawGeometry d
    bindTarget viewport
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    glDisable GL_DEPTH_TEST
    glDisable GL_CULL_FACE
    glUseProgram (programId (screenProgram r))
    intUniform (screenProgram r) "mode" 0
    intUniform (screenProgram r) "texture0" 0
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D (whiteTexture r)
    vectorUniform (screenProgram r) "skyTop" (toLinear (skyTop env))
    vectorUniform (screenProgram r) "skyBottom" (toLinear (skyBottom env))
    drawBuffer GL_TRIANGLES (screenBuffer r)
    glEnable GL_DEPTH_TEST
    glEnable GL_CULL_FACE
    setEnvironment r cam env lightVP (world project)
    forM_ draws $ \d -> do
      let shader=pbrProgram r;mat=drawMaterial d;model=drawMatrix d
      matrixUniform shader "mvp" (vp L.!*! model)
      matrixUniform shader "matModel" model
      matrixUniform shader "matNormal" (L.transpose (L.inv44 model))
      vectorUniform shader "baseColor" (toLinear (albedo mat))
      floatUniform shader "metalness" (metallic mat)
      floatUniform shader "roughness" (roughness mat)
      floatUniform shader "emission" (emission mat)
      glActiveTexture GL_TEXTURE0
      forM_ (modelMeshes (drawModel d)) $ \mesh -> do
        let tex=if drawTexture d==whiteTexture r then maybe (whiteTexture r) id (importedTexture mesh) else drawTexture d
        glBindTexture GL_TEXTURE_2D tex
        drawBuffer GL_TRIANGLES (meshBuffer mesh)
    when edit (drawOverlays r vp (world project) draws grid selected)
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
    pure $ if edit && mouseEnabled mouse && leftPressed mouse && inside rect mouse then pick (screenRay rect cam mouse) draws selected else selected

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
    forM_ (IM.lookup (drawEntity d) (transforms w)) $ \t ->
      forM_ (zip (axisBuffers r) [L.V3 0.9 0.2 0.2,L.V3 0.1 0.9 0.3,L.V3 0.1 0.4 0.95]) $ \(buffer,color) -> line color (trs (toLinear (position t)) (L.V3 1 1 1)) buffer
  forM_ (IM.toAscList (lights w)) $ \(e,_) -> forM_ (IM.lookup e (transforms w)) $ \t -> do
    let radius=if Just e==selected then 0.2 else 0.12
    line (if Just e==selected then accent else L.V3 1 0.84 0.52) (trs (toLinear (position t)) (L.V3 radius radius radius)) (lightBuffer r)

pick :: Ray -> [Draw] -> Maybe Entity -> Maybe Entity
pick ray draws selected = maybe selected (Just . snd) nearest
  where
    hits=[(distance,drawEntity d) | d<-draws,let localRay=objectRay (drawMatrix d) ray,mesh<-modelMeshes (drawModel d),Just distance<-[rayMesh localRay (meshBounds mesh) (meshPositions mesh) (meshIndices mesh)]]
    nearest=case hits of []->Nothing;_->Just (minimum hits)
