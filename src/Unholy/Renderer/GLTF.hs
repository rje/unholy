{-# LANGUAGE OverloadedStrings #-}
-- Metadata only: Aeson preserves float factors that raylib's geometry importer
-- drops or quantizes. Geometry and image decoding remain upstream integrations.
module Unholy.Renderer.GLTF (MaterialInfo(..),defaultInfo,loadMaterials,loadAnimatedMaterials,readDocument,assetDependencies) where

import qualified Data.Aeson as A
import Data.Aeson ((.:?),(.!=))
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import Linear (V3(..))
import System.FilePath (takeExtension,isAbsolute,splitDirectories)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Control.Monad (unless,forM_)

data MaterialInfo = MaterialInfo
  { baseFactor :: !(V3 Float), metalFactor :: !Float, roughFactor :: !Float
  , normalScale :: !Float, emissiveFactor :: !(V3 Float), occlusionStrength :: !Float
  } deriving (Eq,Show)
defaultInfo :: MaterialInfo
defaultInfo=MaterialInfo (V3 1 1 1) 1 1 1 (V3 0 0 0) 1

loadMaterials :: FilePath -> IO (Maybe (IM.IntMap MaterialInfo))
loadMaterials=loadWith False
loadAnimatedMaterials :: FilePath -> IO (Maybe (IM.IntMap MaterialInfo))
loadAnimatedMaterials=loadWith True
loadWith :: Bool -> FilePath -> IO (Maybe (IM.IntMap MaterialInfo))
loadWith animated path=readDocument path >>= traverse (parse (document animated))
  where parse parser value=either (failure path) pure (A.parseEither parser value)

assetDependencies :: FilePath -> IO [FilePath]
assetDependencies path=do
  value<-readDocument path
  maybe (pure []) (either (failure path) pure . A.parseEither dependencies) value
  where
    dependencies=A.withObject "glTF dependencies" $ \o->do
      buffers<-o .:? "buffers" .!= ([]::[A.Value])
      images<-o .:? "images" .!= ([]::[A.Value])
      uris<-mapM (A.withObject "resource" (\resource->resource .:? "uri" .!= "")) (buffers++images)
      let files=filter (\uri->not (null uri) && not ("data:" `isPrefixOf` uri)) uris
      unless (all (\uri->not (isAbsolute uri) && all (/="..") (splitDirectories uri) && all (`notElem` ("%:?#\\"::String)) uri) files) (fail "Resource URIs must be local relative paths without parent traversal or URL escaping")
      pure files

readDocument :: FilePath -> IO (Maybe A.Value)
readDocument path
  | map toLower (takeExtension path) `notElem` [".gltf",".glb"] = pure Nothing
  | otherwise=do
    bytes<-B.readFile path
    payload<-if map toLower (takeExtension path)==".glb" then either (failure path) pure (jsonChunk bytes) else pure bytes
    Just <$> either (failure path) pure (A.eitherDecodeStrict' payload)

failure :: FilePath -> String -> IO a
failure path message=ioError (userError ("glTF materials / "++path++": "++message))

jsonChunk :: B.ByteString -> Either String B.ByteString
jsonChunk bytes=do
  unless (B.length bytes>=20 && word 0==0x46546c67 && word 4==2 && word 8==B.length bytes && word 16==0x4e4f534a) (Left "Invalid GLB 2 JSON header")
  let count=word 12
  unless (count>=0 && count<=B.length bytes-20) (Left "Invalid GLB JSON length")
  pure (B.take count (B.drop 20 bytes))
  where word offset=sum [fromIntegral (B.index bytes (offset+i))*256^i | i<-[0..3::Int]]

document :: Bool -> A.Value -> A.Parser (IM.IntMap MaterialInfo)
document animated=A.withObject "glTF" $ \o->do
  required<-o .:? "extensionsRequired" .!= ([]::[String])
  unless (null required) (fail "Required extensions are not supported")
  skins<-o .:? "skins" .!= ([]::[A.Value])
  unless (animated || null skins) (fail "Skinned assets require an Animator component")
  samplers<-o .:? "samplers" .!= ([]::[A.Value])
  forM_ samplers $ A.withObject "sampler" $ \s->do
    wrapS<-s .:? "wrapS" .!= (10497::Int);wrapT<-s .:? "wrapT" .!= (10497::Int)
    mag<-s .:? "magFilter" .!= (9729::Int);small<-s .:? "minFilter" .!= (9987::Int)
    unless (wrapS==10497 && wrapT==10497 && mag==9729 && small==9987) (fail "Only repeat wrapping and linear mipmap texture sampling are supported")
  meshes<-o .:? "meshes" .!= ([]::[A.Value])
  forM_ meshes $ A.withObject "mesh" $ \m->do
    primitives<-m .:? "primitives" .!= ([]::[A.Value])
    forM_ primitives $ A.withObject "primitive" $ \p->do
      mode<-p .:? "mode" .!= (4::Int)
      targets<-p .:? "targets" .!= ([]::[A.Value])
      unless (mode==4 && null targets) (fail "Only static triangle primitives are supported")
      attrs<-p .:? "attributes" .!= (KM.empty :: KM.KeyMap A.Value)
      unless (not (KM.member "COLOR_0" attrs)) (fail "Vertex colors are not supported")
  values<-o .:? "materials" .!= ([]::[A.Value])
  IM.fromList . zip [1..] <$> mapM material values

material :: A.Value -> A.Parser MaterialInfo
material=A.withObject "material" $ \o->do
  mode<-o .:? "alphaMode" .!= ("OPAQUE"::String)
  doubleSided<-o .:? "doubleSided" .!= False
  extensions<-o .:? "extensions" .!= (KM.empty :: KM.KeyMap A.Value)
  unless (mode=="OPAQUE" && not doubleSided && KM.null extensions) (fail "Only opaque, single-sided core materials are supported")
  pbr<-o .:? "pbrMetallicRoughness" .!= A.Object KM.empty
  (base,metal,rough)<-A.withObject "pbrMetallicRoughness" (\p->do
    color<-p .:? "baseColorFactor" .!= [1,1,1,1]
    base<-case color of [r,g,b,a] | a==1->pure (V3 r g b);_->fail "Expected opaque RGBA factor"
    metal<-p .:? "metallicFactor" .!= 1
    rough<-p .:? "roughnessFactor" .!= 1
    mapM_ (textureInfo p) ["baseColorTexture","metallicRoughnessTexture"]
    pure (base,metal,rough)) pbr
  mapM_ (textureInfo o) ["normalTexture","emissiveTexture","occlusionTexture"]
  normal<-nestedScalar o "normalTexture" "scale" 1
  occlusion<-nestedScalar o "occlusionTexture" "strength" 1
  emissive<-o .:? "emissiveFactor" .!= [0,0,0]
  glow<-case emissive of [r,g,b]->pure (V3 r g b);_->fail "Expected RGB emissive factor"
  let channels (V3 r g b)=[r,g,b]
      unit n=not (isNaN n || isInfinite n) && n>=0 && n<=1
  unless (all unit (channels base++channels glow++[metal,rough,occlusion]) && normal>=0 && not (isNaN normal || isInfinite normal)) (fail "Invalid material factors")
  pure (MaterialInfo base metal rough normal glow occlusion)
  where
    textureInfo o key=do
      value<-o .:? key
      forM_ value $ A.withObject "texture info" $ \t->do
        coord<-t .:? "texCoord" .!= (0::Int)
        extensions<-t .:? "extensions" .!= (KM.empty :: KM.KeyMap A.Value)
        unless (coord==0 && KM.null extensions) (fail "Only untransformed UV0 textures are supported")
    nestedScalar o key name fallback=do
      value<-o .:? key
      maybe (pure fallback) (A.withObject "texture info" (\t->t .:? name .!= fallback)) value
