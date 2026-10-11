{-# LANGUAGE ForeignFunctionInterface #-}
module Unholy.Renderer.Assets where

import qualified Unholy.Renderer.Primitives as P
import qualified Unholy.Renderer.GLTF as F
import Unholy.Renderer.GL
import Unholy.Renderer.Camera (Bounds, boundsOf)
import Graphics.GL
import Foreign
import Foreign.C
import Control.Exception (bracket, onException,try,IOException)
import Control.Monad (forM_, when, unless)
import qualified Data.Vector.Storable as VS
import Linear.Matrix (identity)
import Linear (M44, V3(..), V4(..), transpose)
import qualified Data.IntMap.Strict as IM

data ImportedMaterial = ImportedMaterial { materialInfo :: !F.MaterialInfo, materialTextures :: ![Maybe GLuint] }
data MeshData = MeshData { meshBuffer :: !Buffer, meshPositions :: !(VS.Vector (V3 Float)), meshIndices :: !(VS.Vector Word16), meshBounds :: !Bounds, importedMaterial :: !(Maybe ImportedMaterial) }
importedTexture :: MeshData -> Maybe GLuint
importedTexture mesh=importedMaterial mesh >>= textureSlot 0
textureSlot :: Int -> ImportedMaterial -> Maybe GLuint
textureSlot index material=case drop index (materialTextures material) of value:_->value;_->Nothing
meshTextures :: MeshData -> [GLuint]
meshTextures mesh=maybe [] (concatMap (maybe [] (:[])) . materialTextures) (importedMaterial mesh)
data ModelData = ModelData { modelMeshes :: ![MeshData], importTransform :: !(M44 Float), modelBounds :: !Bounds, materialError :: !(Maybe String) }

foreign import ccall unsafe "hj_asset_model" cModel :: CString -> IO (Ptr ())
foreign import ccall unsafe "hj_asset_model_free" cFreeModel :: Ptr () -> IO ()
foreign import ccall unsafe "hj_asset_mesh_count" cMeshCount :: Ptr () -> IO CInt
foreign import ccall unsafe "hj_asset_mesh" cMesh :: Ptr () -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO CInt
foreign import ccall unsafe "hj_asset_transform" cTransform :: Ptr () -> Ptr Float -> IO ()
foreign import ccall unsafe "hj_asset_image" cImage :: CString -> Ptr CInt -> IO (Ptr ())
foreign import ccall unsafe "hj_asset_pixels" cPixels :: Ptr () -> IO (Ptr Word8)
foreign import ccall unsafe "hj_asset_image_free" cFreeImage :: Ptr () -> IO ()
foreign import ccall unsafe "hj_asset_material_index" cMaterialIndex :: Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "hj_asset_mesh_image" cMeshImage :: Ptr () -> CInt -> CInt -> Ptr CInt -> IO (Ptr ())

loadModel :: FilePath -> IO ModelData
loadModel=loadModelWith False
loadModelWith :: Bool -> FilePath -> IO ModelData
loadModelWith _ path | Just cpu<-P.primitive path = do
  let raw=P.vertices cpu
      positions=VS.generate (VS.length raw `div` 8) (\i -> V3 (raw VS.! (i*8)) (raw VS.! (i*8+1)) (raw VS.! (i*8+2)))
      bounds=boundsOf positions
  gpu<-uploadBuffer raw (P.indices cpu)
  pure (ModelData [MeshData gpu positions (P.indices cpu) bounds Nothing] identity bounds Nothing)
loadModelWith animated path = withCString path $ \name -> bracket (cModel name) cFreeModel $ \p -> do
  when (p==nullPtr) (ioError (userError ("Cannot import mesh: "++path)))
  attempted<-try ((if animated then F.loadAnimatedMaterials else F.loadMaterials) path)
  let metadata=either (const Nothing) id attempted
      diagnostic=either (Just . show . (id :: IOException -> IOException)) (const Nothing) attempted
  transform<-allocaArray 16 $ \out -> do
    cTransform p out
    columns<-peekArray 16 out
    case columns of
      [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q]->pure (transpose (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o q)))
      _->ioError (userError "Invalid importer transform")
  count<-fromIntegral <$> cMeshCount p
  let loadAll []=pure []
      loadAll (i:is)=do
        slot<-fromIntegral <$> cMaterialIndex p (fromIntegral i)
        let info=fmap (IM.findWithDefault F.defaultInfo slot) metadata
        mesh<-loadMesh p i info
        rest<-loadAll is `onException` deleteMesh mesh
        pure (mesh:rest)
  meshes<-loadAll [0..count-1]
  unless (not (null meshes)) (ioError (userError ("Empty model: "++path)))
  let bounds=boundsOf (VS.fromList (concatMap (\m -> let (lo,hi)=meshBounds m in [lo,hi]) meshes))
  pure (ModelData meshes transform bounds diagnostic)

loadMesh :: Ptr () -> Int -> Maybe F.MaterialInfo -> IO MeshData
loadMesh model index info = allocaArray 2 $ \counts -> allocaArray 4 $ \arrays -> do
  valid<-cMesh model (fromIntegral index) counts arrays
  unless (valid/=0) (ioError (userError "Imported mesh has no vertices"))
  vertexCount<-fromIntegral <$> peekElemOff counts 0
  indexCount<-fromIntegral <$> peekElemOff counts 1
  positionsPtr<-castPtr <$> peekElemOff arrays 0
  uvPtr<-castPtr <$> peekElemOff arrays 1
  normalsPtr<-castPtr <$> peekElemOff arrays 2
  indicesPtr<-castPtr <$> peekElemOff arrays 3
  unless (vertexCount>0 && indexCount>0) (ioError (userError "Imported mesh has no triangles"))
  -- Copy into Haskell-owned contiguous arrays before releasing the importer.
  rawPositions<-copyVector (vertexCount*3) positionsPtr
  uvs<-if uvPtr==nullPtr then pure (VS.replicate (vertexCount*2) 0) else copyVector (vertexCount*2) uvPtr
  normals<-if normalsPtr==nullPtr then pure (VS.generate (vertexCount*3) (\i -> if i `mod` 3==1 then 1 else 0)) else copyVector (vertexCount*3) normalsPtr
  indices<-if indicesPtr==nullPtr then pure VS.empty else copyVector indexCount indicesPtr
  when (VS.any (\i -> fromIntegral i>=vertexCount) indices) (ioError (userError "Imported mesh has invalid indices"))
  let positions=VS.generate vertexCount (\i -> V3 (rawPositions VS.! (i*3)) (rawPositions VS.! (i*3+1)) (rawPositions VS.! (i*3+2)))
      vertices=VS.generate (vertexCount*8) $ \i -> let (v,component)=i `divMod` 8 in
        if component<3 then rawPositions VS.! (v*3+component)
        else if component<5 then uvs VS.! (v*2+component-3)
        else normals VS.! (v*3+component-5)
  gpu<-uploadBuffer vertices indices
  textures<-loadMaps [0..5] `onException` deleteBuffer gpu
  pure (MeshData gpu positions indices (boundsOf positions) (Just (ImportedMaterial (maybe F.defaultInfo {F.metalFactor=0,F.roughFactor=0.4} id info) textures)))
  where
    loadMaps []=pure []
    loadMaps (slot:rest)=do
      tex<-loadImportedTexture model index slot
      following<-loadMaps rest `onException` mapM_ (deleteObject glDeleteTextures) tex
      pure (tex:following)

loadImportedTexture :: Ptr () -> Int -> Int -> IO (Maybe GLuint)
loadImportedTexture model index slot = allocaArray 2 $ \size -> bracket (cMeshImage model (fromIntegral index) (fromIntegral slot) size) cFreeImage $ \p ->
  if p==nullPtr then pure Nothing else do
    w<-fromIntegral <$> peekElemOff size 0
    h<-fromIntegral <$> peekElemOff size 1
    Just <$> (cPixels p >>= newTextureWithColorSpace (slot `elem` [0,4]) w h)

copyVector :: Storable a => Int -> Ptr a -> IO (VS.Vector a)
copyVector n p = do
  foreignPtr<-mallocForeignPtrArray n
  withForeignPtr foreignPtr $ \out -> copyArray out p n
  pure (VS.unsafeFromForeignPtr0 foreignPtr n)
deleteModel :: ModelData -> IO ()
deleteModel model = forM_ (modelMeshes model) deleteMesh
deleteMesh :: MeshData -> IO ()
deleteMesh mesh = deleteBuffer (meshBuffer mesh) >> mapM_ (deleteObject glDeleteTextures) (meshTextures mesh)

loadTexture :: FilePath -> IO GLuint
loadTexture path = withCString path $ \name -> allocaArray 2 $ \size -> bracket (cImage name size) cFreeImage $ \p -> do
  when (p==nullPtr) (ioError (userError ("Cannot decode texture: "++path)))
  w<-fromIntegral <$> peekElemOff size 0
  h<-fromIntegral <$> peekElemOff size 1
  cPixels p >>= newTextureWithColorSpace True w h
