{-# LANGUAGE ForeignFunctionInterface #-}
-- Shared headless geometry decoding. No window, importer GPU objects or draws.
module Unholy.Asset.CPU (loadTriangles) where

import qualified Unholy.Renderer.Primitives as P
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C
import Linear (V3(..))
import Control.Exception (bracket)
import Control.Monad (unless)
import System.FilePath (takeExtension)
import Data.Char (toLower)

foreign import ccall unsafe "uh_gltf_load" cLoad :: CString -> IO (Ptr ())
foreign import ccall unsafe "uh_gltf_free" cFree :: Ptr () -> IO ()
foreign import ccall unsafe "uh_gltf_triangles" cTriangles :: Ptr () -> Ptr Float -> IO CInt

loadTriangles :: FilePath -> IO (VS.Vector (V3 Float))
loadTriangles path | Just cpu<-P.primitive path = do
  let raw=P.vertices cpu
      vertex i=V3 (raw VS.! (i*8)) (raw VS.! (i*8+1)) (raw VS.! (i*8+2))
  pure (if VS.null (P.indices cpu) then VS.generate (VS.length raw `div` 8) vertex else VS.map (vertex . fromIntegral) (P.indices cpu))
loadTriangles path=do
  unless (map toLower (takeExtension path) `elem` [".gltf",".glb"]) (ioError (userError ("CPU mesh colliders support glTF/GLB and built-in primitives: "++path)))
  withCString path $ \name->bracket (cLoad name) (\p->unless (p==nullPtr) (cFree p)) $ \p->do
    unless (p/=nullPtr) (ioError (userError ("Cannot decode collider mesh: "++path)))
    count<-fromIntegral <$> cTriangles p nullPtr
    unless (count>0 && count<=1000000) (ioError (userError ("Collider requires unskinned triangle geometry: "++path)))
    storage<-mallocForeignPtrArray count
    result<-withForeignPtr storage $ \out->cTriangles p (castPtr out)
    unless (fromIntegral result==count) (ioError (userError ("Invalid collider accessor data: "++path)))
    let positions=VS.unsafeFromForeignPtr0 storage count
    unless (VS.all (\(V3 x y z)->all (\v->not (isNaN v || isInfinite v)) [x,y,z]) positions) (ioError (userError ("Nonfinite collider mesh: "++path)))
    pure positions
