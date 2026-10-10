{-# LANGUAGE ForeignFunctionInterface #-}
module HJ.Renderer.Assets where

import HJ.Renderer.GL
import HJ.Renderer.Camera (Bounds, boundsOf)
import Graphics.GL
import Foreign
import Foreign.C
import Control.Exception (bracket, onException)
import Control.Monad (forM_, when, unless)
import qualified Data.Vector.Storable as VS
import Linear (M44, V3(..), V4(..), transpose)

data MeshData = MeshData { meshBuffer :: !Buffer, meshPositions :: !(VS.Vector (V3 Float)), meshIndices :: !(VS.Vector Word16), meshBounds :: !Bounds, importedTexture :: !(Maybe GLuint) }
data ModelData = ModelData { modelMeshes :: ![MeshData], importTransform :: !(M44 Float), modelBounds :: !Bounds }

foreign import ccall unsafe "hj_asset_model" cModel :: CString -> IO (Ptr ())
foreign import ccall unsafe "hj_asset_model_free" cFreeModel :: Ptr () -> IO ()
foreign import ccall unsafe "hj_asset_mesh_count" cMeshCount :: Ptr () -> IO CInt
foreign import ccall unsafe "hj_asset_mesh" cMesh :: Ptr () -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO CInt
foreign import ccall unsafe "hj_asset_transform" cTransform :: Ptr () -> Ptr Float -> IO ()
foreign import ccall unsafe "hj_asset_image" cImage :: CString -> Ptr CInt -> IO (Ptr ())
foreign import ccall unsafe "hj_asset_pixels" cPixels :: Ptr () -> IO (Ptr Word8)
foreign import ccall unsafe "hj_asset_image_free" cFreeImage :: Ptr () -> IO ()
foreign import ccall unsafe "hj_asset_mesh_image" cMeshImage :: Ptr () -> CInt -> Ptr CInt -> IO (Ptr ())

loadModel :: FilePath -> IO ModelData
loadModel path = withCString path $ \name -> bracket (cModel name) cFreeModel $ \p -> do
  when (p==nullPtr) (ioError (userError ("Cannot import mesh: "++path)))
  transform<-allocaArray 16 $ \out -> do
    cTransform p out
    columns<-peekArray 16 out
    case columns of
      [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q]->pure (transpose (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o q)))
      _->ioError (userError "Invalid importer transform")
  count<-fromIntegral <$> cMeshCount p
  let loadAll []=pure []
      loadAll (i:is)=do
        mesh<-loadMesh p i
        rest<-loadAll is `onException` deleteMesh mesh
        pure (mesh:rest)
  meshes<-loadAll [0..count-1]
  unless (not (null meshes)) (ioError (userError ("Empty model: "++path)))
  let bounds=boundsOf (VS.fromList (concatMap (\m -> let (lo,hi)=meshBounds m in [lo,hi]) meshes))
  pure (ModelData meshes transform bounds)

loadMesh :: Ptr () -> Int -> IO MeshData
loadMesh model index = allocaArray 2 $ \counts -> allocaArray 4 $ \arrays -> do
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
  tex<-loadImportedTexture model index `onException` deleteBuffer gpu
  pure (MeshData gpu positions indices (boundsOf positions) tex)

loadImportedTexture :: Ptr () -> Int -> IO (Maybe GLuint)
loadImportedTexture model index = allocaArray 2 $ \size -> bracket (cMeshImage model (fromIntegral index) size) cFreeImage $ \p ->
  if p==nullPtr then pure Nothing else do
    w<-fromIntegral <$> peekElemOff size 0
    h<-fromIntegral <$> peekElemOff size 1
    Just <$> (cPixels p >>= newTexture w h)

copyVector :: Storable a => Int -> Ptr a -> IO (VS.Vector a)
copyVector n p = do
  foreignPtr<-mallocForeignPtrArray n
  withForeignPtr foreignPtr $ \out -> copyArray out p n
  pure (VS.unsafeFromForeignPtr0 foreignPtr n)
deleteModel :: ModelData -> IO ()
deleteModel model = forM_ (modelMeshes model) deleteMesh
deleteMesh :: MeshData -> IO ()
deleteMesh mesh = deleteBuffer (meshBuffer mesh) >> mapM_ (deleteObject glDeleteTextures) (importedTexture mesh)

loadTexture :: FilePath -> IO GLuint
loadTexture path = withCString path $ \name -> allocaArray 2 $ \size -> bracket (cImage name size) cFreeImage $ \p -> do
  when (p==nullPtr) (ioError (userError ("Cannot decode texture: "++path)))
  w<-fromIntegral <$> peekElemOff size 0
  h<-fromIntegral <$> peekElemOff size 1
  cPixels p >>= newTexture w h
