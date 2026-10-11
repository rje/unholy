{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- One CPU glTF description feeds Haskell animation, GPU upload and picking.
module Unholy.Asset.GLTF
  ( Rig,
    RigMesh,
    Image (..),
    loadAsset,
    rigMeshes,
    rigNodes,
    rigSkins,
    rigClips,
    rigImages,
    rigMaterialInfos,
    rigMaterialImages,
    rigMaterialError,
    bindVertices,
    triangleIndices,
    vertexJoints,
    vertexWeights,
    meshSkin,
    meshNode,
    meshMaterial,
    influenceBounds,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C
import Linear
import System.FilePath (takeDirectory, (</>))
import Unholy.Asset.GLTF.Internal
import qualified Unholy.Renderer.GLTF as F
import Unholy.Types (finite)

data RawMesh = RawMesh Int (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) Int (Maybe Int)

data RawChannel = RawChannel Int String Bool Int Int

data RawClip = RawClip String [RawChannel]

data Document = Document [Node] [(Int, Int)] [([Int], Maybe Int)] [RawMesh] [RawClip]

foreign import ccall unsafe "uh_gltf_load" cLoad :: CString -> IO (Ptr ())

foreign import ccall unsafe "uh_gltf_free" cFree :: Ptr () -> IO ()

foreign import ccall unsafe "uh_gltf_accessor" cAccessor :: Ptr () -> CInt -> Ptr CInt -> Ptr Float -> IO CInt

loadAsset :: FilePath -> IO Rig
loadAsset path = do
  _ <- F.assetDependencies path
  value <- F.readDocument path >>= maybe (bad "Animation assets must be glTF/GLB") pure
  Document nodes links skins meshes clips <- either bad pure (A.parseEither document value)
  unless (not (null nodes) && length nodes <= 512 && not (null meshes)) (bad "Empty or oversized rig")
  let parents = IM.fromList links
      ns = V.fromList [n {nodeParent = IM.findWithDefault (-1) i parents} | (i, n) <- zip [0 ..] nodes]
      validNode i = i >= 0 && i < V.length ns
      chain seen i
        | i < 0 = Just ()
        | not (validNode i) || i `elem` seen = Nothing
        | otherwise = chain (i : seen) (nodeParent (ns V.! i))
  unless (length links == IM.size parents && all (\i -> chain [] i == Just ()) [0 .. V.length ns - 1]) (bad "Invalid rig hierarchy")
  withCString path $ \name -> bracket (cLoad name) (\p -> unless (p == nullPtr) (cFree p)) $ \p -> do
    unless (p /= nullPtr) (bad "cgltf rejected animation data")
    let readArray index size = allocaArray 2 $ \counts -> do
          ok <- cAccessor p (fromIntegral index) counts nullPtr
          unless (ok /= 0) (bad "Invalid animation accessor")
          count <- fromIntegral <$> peekElemOff counts 0
          components <- fromIntegral <$> peekElemOff counts 1
          unless (ok /= 0 && count > 0 && components == size && count * size <= 1000000) (bad "Invalid animation accessor layout")
          ptr <- mallocForeignPtrArray (count * size)
          copied <- withForeignPtr ptr (cAccessor p (fromIntegral index) counts)
          let result = VS.unsafeFromForeignPtr0 ptr (count * size)
          unless (copied /= 0 && VS.all finite result) (bad "Invalid or nonfinite animation accessor")
          pure result
    ss <- forM skins $ \(joints, index) -> do
      unless (not (null joints) && length joints <= 512 && length joints == length (nub joints) && all validNode joints) (bad "Invalid skin joints")
      matrices <-
        maybe
          (pure (replicate (length joints) identity))
          ( \a -> do
              raw <- readArray a 16
              unless (VS.length raw >= length joints * 16) (bad "Inverse bind count mismatch")
              pure [matrix (VS.toList (VS.slice (i * 16) 16 raw)) | i <- [0 .. length joints - 1]]
          )
          index
      unless (all (\m -> abs (det44 m) > 1e-8) matrices) (bad "Singular inverse bind matrix")
      pure (Skin joints matrices)
    ms <- forM meshes $ \(RawMesh pos normal uv joints weights indices skin nodeIndex material) -> do
      unless (maybe True (\skinIndex -> skinIndex >= 0 && skinIndex < length ss) skin) (bad "Invalid mesh skin")
      ps <- readArray pos 3
      let count = VS.length ps `div` 3
      unless (count <= 65535) (bad "Primitive exceeds 65535 vertices")
      is <- maybe (pure (VS.generate count fromIntegral)) (\a -> readArray a 1) indices
      unless (VS.length is `mod` 3 == 0 && VS.all (integerIn count) is) (bad "Invalid triangle indices")
      normals <- mapM (\index -> readArray index 3) normal
      uvs <- maybe (pure (VS.replicate (count * 2) 0)) (\index -> readArray index 2) uv
      unless (maybe True ((== count * 3) . VS.length) normals && VS.length uvs == count * 2) (bad "Geometry attribute counts disagree")
      (js, ws) <- case skin of
        Nothing -> pure (VS.replicate count (V4 0 0 0 0), VS.replicate count (V4 1 0 0 0))
        Just skinIndex -> do
          j <- maybe (bad "Skin requires JOINTS_0") (\index -> readArray index 4) joints
          w <- maybe (bad "Skin requires WEIGHTS_0") (\index -> readArray index 4) weights
          unless (VS.length j == count * 4 && VS.length w == count * 4 && VS.all (integerIn (length (skinJoints (ss !! skinIndex)))) j) (bad "Invalid joint indices/counts")
          let weights4 = VS.generate count (\i -> v4At w (i * 4))
          unless (VS.all (\v -> all (>= 0) v && abs (sum v - 1) < 0.005) weights4) (bad "Invalid skin weights")
          pure (VS.generate count (\i -> fmap round (v4At j (i * 4))), VS.map (\v -> v ^/ sum v) weights4)
      -- glTF requires flat shading when NORMAL is absent. Split every triangle
      -- corner, including its UV and skin influences, before generating normals.
      let corners = VS.map round is
          expand size values = VS.generate (VS.length corners * size) $ \i -> let (v, c) = i `divMod` size in values VS.! (corners VS.! v * size + c)
          (positions, textureCoordinates, jointIndices, skinWeights, faceNormals, triangles) = case normals of
            Just ns -> (ps, uvs, js, ws, ns, VS.map fromIntegral corners)
            Nothing ->
              let expanded = expand 3 ps
               in ( expanded,
                    expand 2 uvs,
                    VS.map (js VS.!) corners,
                    VS.map (ws VS.!) corners,
                    flatNormals expanded,
                    VS.generate (VS.length corners) fromIntegral
                  )
          vertexCount = VS.length positions `div` 3
      unless (vertexCount <= 65535) (bad "Primitive exceeds 65535 vertices after flat-normal expansion")
      unless (VS.all (\i -> norm (v3At faceNormals (i * 3)) > 1e-6) (VS.generate vertexCount id)) (bad "Invalid normals")
      let raw = VS.generate (vertexCount * 8) $ \i -> let (v, c) = i `divMod` 8 in if c < 3 then positions VS.! (v * 3 + c) else if c < 5 then textureCoordinates VS.! (v * 2 + c - 3) else faceNormals VS.! (v * 3 + c - 5)
          bounds =
            IM.fromListWith
              unionBounds
              [(fromIntegral joint, (point, point)) | i <- [0 .. vertexCount - 1], let point = v3At positions (i * 3), (joint, weight) <- zip (toList4 (jointIndices VS.! i)) (toList4 (skinWeights VS.! i)), weight > 0]
      pure (RigMesh raw triangles jointIndices skinWeights skin nodeIndex material bounds)
    cs <- forM clips $ \(RawClip name channels) -> do
      chs <- forM channels $ \(RawChannel node path' step input output) -> do
        unless (validNode node && not (matrixNode (ns V.! node))) (bad "Animation targets require a TRS node")
        ts <- readArray input 1
        vs <- readArray output (if path' == "rotation" then 4 else 3)
        unless (VS.head ts >= 0 && and (zipWith (<) (VS.toList ts) (drop 1 (VS.toList ts))) && VS.length vs == VS.length ts * (if path' == "rotation" then 4 else 3)) (bad "Animation keys must be strictly ordered with matching values")
        let values = VS.generate (VS.length ts) (\i -> if path' == "rotation" then v4At vs (i * 4) else let V3 x y z = v3At vs (i * 3) in V4 x y z 0)
        unless (path' /= "rotation" || VS.all (\v -> abs (norm v - 1) < 0.005) values) (bad "Rotation keys must be unit quaternions")
        unless (path' /= "scale" || VS.all (\(V4 x y z _) -> all (> 0) [x, y, z]) values) (bad "Animated scale must stay positive")
        pure (Channel node path' step ts values)
      unless (not (null chs) && length [(channelNode c, channelPath c) | c <- chs] == M.size (M.fromList [((channelNode c, channelPath c), ()) | c <- chs])) (bad "Empty clip or duplicate animation channel")
      pure (name, Clip (maximum (map (VS.last . keyTimes) chs)) chs)
    unless (length cs == M.size (M.fromList cs)) (bad "Clip names must be unique")
    images <- loadImages path p value
    let info = F.parseMaterials True value
        materialInfos = either (const IM.empty) id info
        diagnostic = either Just (const Nothing) info
    slots <- either bad pure (A.parseEither (imageSlots (V.length images)) value)
    unless (all (\mesh -> maybe True (\i -> i >= 0 && IM.member i slots) (meshMaterial mesh)) ms) (bad "Invalid material index")
    let order = nub (concatMap (ancestors ns) [0 .. V.length ns - 1])
    pure (Rig ns (V.fromList order) (V.fromList ss) ms (M.fromList cs) images materialInfos slots diagnostic)
  where
    bad msg = ioError (userError (path ++ ": " ++ msg))
    integerIn count n = n >= 0 && n < fromIntegral count && n == fromIntegral (round n :: Int)

document :: A.Value -> A.Parser Document
document = A.withObject "animated glTF" $ \o -> do
  required <- o .:? "extensionsRequired" .!= ([] :: [String])
  unless (null required) (fail "Required extensions are not supported")
  rawNodes <- o .: "nodes"
  rawMeshes <- o .: "meshes"
  rawSkins <- o .:? "skins" .!= []
  rawAccessors <- o .: "accessors"
  scenes <- o .:? "scenes" .!= ([] :: [A.Value])
  sceneIndex <- o .:? "scene" .!= (0 :: Int)
  roots <-
    if null scenes
      then pure [0 .. length rawNodes - 1]
      else do
        unless (sceneIndex >= 0 && sceneIndex < length scenes) (fail "Invalid default scene")
        A.withObject "scene" (\scene -> scene .:? "nodes" .!= []) (scenes !! sceneIndex)
  nodes <- mapM node rawNodes
  children <- mapM (A.withObject "node" (\n -> n .:? "children" .!= [])) rawNodes
  let descendants seen i
        | i < 0 || i >= length rawNodes || i `elem` seen = []
        | otherwise = i : concatMap (descendants (i : seen)) (children !! i)
      selected = nub (concatMap (descendants []) roots)
  unless (all (\i -> i >= 0 && i < length rawNodes) roots) (fail "Invalid scene roots")
  instances <- fmap concat $ forM selected $ \nodeIndex ->
    A.withObject
      "node"
      ( \n -> do
          m <- n .:? "mesh"
          case m of
            Nothing -> pure []
            Just i -> do
              skin <- n .:? "skin"
              unless (i >= 0 && i < length rawMeshes) (fail "Invalid mesh node")
              A.withObject
                "mesh"
                ( \mesh -> do
                    ps <- mesh .: "primitives"
                    mapM (primitive skin nodeIndex) ps
                )
                (rawMeshes !! i)
      )
      (rawNodes !! nodeIndex)
  skins <- mapM (A.withObject "skin" (\s -> (,) <$> s .: "joints" <*> s .:? "inverseBindMatrices")) rawSkins
  forM_ instances $ \(RawMesh p n uv j w indices _ _ _) -> do
    let check :: Int -> String -> [Int] -> A.Parser ()
        check index layout allowed = do
          unless (index >= 0 && index < length rawAccessors) (fail "Invalid geometry accessor")
          A.withObject
            "geometry accessor"
            ( \a -> do
                component <- a .: "componentType"
                kind <- a .: "type"
                normalized <- a .:? "normalized" .!= False
                unless (kind == layout && component `elem` allowed && (layout /= "VEC4" || component /= 5126 || not normalized)) (fail "Unsupported accessor layout")
                when (layout == "VEC4" && index == maybe (-1) id w && component /= 5126 && not normalized) (fail "Integer weights must be normalized")
            )
            (rawAccessors !! index)
    check p "VEC3" [5126]
    mapM_ (\i -> check i "VEC3" [5126]) n
    mapM_ (\i -> check i "VEC2" [5126, 5121, 5123]) uv
    mapM_ (\i -> check i "VEC4" [5121, 5123]) j
    mapM_ (\i -> check i "VEC4" [5126, 5121, 5123]) w
    mapM_ (\i -> check i "SCALAR" [5121, 5123, 5125]) indices
  animations <- o .:? "animations" .!= []
  clips <- forM (zip [0 :: Int ..] animations) $ \(i, value) ->
    A.withObject
      "animation"
      ( \a -> do
          name <- a .:? "name" .!= ("clip-" ++ show i)
          samplers <- a .: "samplers"
          channels <- a .: "channels"
          parsed <- forM channels $ A.withObject "channel" $ \c -> do
            sampler <- c .: "sampler"
            target <- c .: "target"
            unless (sampler >= 0 && sampler < length samplers) (fail "Invalid animation sampler")
            (n, path) <- A.withObject "target" (\t -> (,) <$> t .: "node" <*> t .: "path") target
            unless (path `elem` ["translation", "rotation", "scale"]) (fail "Morph animation is not supported")
            A.withObject
              "sampler"
              ( \s -> do
                  mode <- s .:? "interpolation" .!= ("LINEAR" :: String)
                  unless (mode `elem` ["LINEAR", "STEP"]) (fail "Only LINEAR and STEP animation are supported")
                  RawChannel n path (mode == "STEP") <$> s .: "input" <*> s .: "output"
              )
              (samplers !! sampler)
          pure (RawClip name parsed)
      )
      value
  pure (Document nodes [(child, parent) | (parent, cs) <- zip [0 ..] children, child <- cs] skins instances clips)
  where
    node = A.withObject "node" $ \n -> do
      t <- n .:? "translation" .!= [0, 0, 0]
      r <- n .:? "rotation" .!= [0, 0, 0, 1]
      s <- n .:? "scale" .!= [1, 1, 1]
      m <- n .:? "matrix"
      unless (length t == 3 && length r == 4 && length s == 3 && all finite (t ++ r ++ s ++ maybe [] id m) && all (> 0) s && abs (norm (v4 r) - 1) < 0.005 && maybe True ((== 16) . length) m) (fail "Invalid node transform")
      let q = quaternion (v4 r)
          translation = v3 t
          scaling = v3 s
          transform = maybe (trs translation q scaling) matrix m
      unless (abs (det44 transform) > 1e-8) (fail "Singular node transform")
      pure (Node (-1) transform translation q scaling (maybe False (const True) m))
    primitive skin nodeIndex = A.withObject "primitive" $ \p -> do
      attrs <- p .: "attributes"
      keys <- A.withObject "attributes" (pure . map K.toString . KM.keys) attrs
      unless (all (`elem` ["POSITION", "NORMAL", "TEXCOORD_0", "JOINTS_0", "WEIGHTS_0", "TANGENT"]) keys) (fail "Unsupported skin attribute")
      extraJoints <- A.withObject "attributes" (\a -> a .:? "JOINTS_1" :: A.Parser (Maybe Int)) attrs
      extraWeights <- A.withObject "attributes" (\a -> a .:? "WEIGHTS_1" :: A.Parser (Maybe Int)) attrs
      unless (extraJoints == Nothing && extraWeights == Nothing) (fail "Only four skin influences are supported")
      mode <- p .:? "mode" .!= (4 :: Int)
      targets <- p .:? "targets" .!= ([] :: [A.Value])
      unless (mode == 4 && null targets) (fail "Only triangle primitives without morphs are supported")
      let get k = A.withObject "attributes" (.: k) attrs
          optional k = A.withObject "attributes" (.:? k) attrs
      RawMesh <$> get "POSITION" <*> optional "NORMAL" <*> optional "TEXCOORD_0" <*> optional "JOINTS_0" <*> optional "WEIGHTS_0" <*> p .:? "indices" <*> pure skin <*> pure nodeIndex <*> p .:? "material"

matrix :: [Float] -> M44 Float
matrix xs = transpose (V4 (row 0) (row 4) (row 8) (row 12)) where row i = v4 (take 4 (drop i xs))

v3 :: [Float] -> V3 Float
v3 [x, y, z] = V3 x y z
v3 _ = V3 0 0 0

v4 :: [Float] -> V4 Float
v4 [x, y, z, w] = V4 x y z w
v4 _ = V4 0 0 0 1

v3At :: VS.Vector Float -> Int -> V3 Float
v3At a i = V3 (a VS.! i) (a VS.! (i + 1)) (a VS.! (i + 2))

v4At :: VS.Vector Float -> Int -> V4 Float
v4At a i = V4 (a VS.! i) (a VS.! (i + 1)) (a VS.! (i + 2)) (a VS.! (i + 3))

quaternion :: V4 Float -> Quaternion Float
quaternion (V4 x y z w) = normalize (Quaternion w (V3 x y z))

trs :: V3 Float -> Quaternion Float -> V3 Float -> M44 Float
trs t q (V3 x y z) = mkTransformation q t !*! V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)

ancestors :: V.Vector Node -> Int -> [Int]
ancestors nodes i = let parent = nodeParent (nodes V.! i) in (if parent < 0 then [] else ancestors nodes parent) ++ [i]

unionBounds :: Bounds -> Bounds -> Bounds
unionBounds (lo, hi) (lo', hi') = (liftI2 min lo lo', liftI2 max hi hi')

toList4 :: V4 a -> [a]
toList4 (V4 x y z w) = [x, y, z, w]

flatNormals :: VS.Vector Float -> VS.Vector Float
flatNormals positions = VS.generate (VS.length positions) $ \i -> let (v, c) = i `divMod` 3; V3 x y z = normals VS.! (v `div` 3) in [x, y, z] !! c
  where
    normals = VS.generate (VS.length positions `div` 9) $ \face ->
      let a = face * 3; n = cross (point (a + 1) - point a) (point (a + 2) - point a)
       in if norm n > 1e-8 then normalize n else V3 0 1 0
    point i = v3At positions (i * 3)

foreign import ccall unsafe "uh_gltf_image_bytes" cImageBytes :: Ptr () -> CInt -> Ptr Word8 -> IO CInt

loadImages :: FilePath -> Ptr () -> A.Value -> IO (V.Vector Image)
loadImages path p value = do
  specs <- either (ioError . userError) pure (A.parseEither parser value)
  V.fromList
    <$> forM
      (zip [0 :: Int ..] specs)
      ( \(i, (uri, mime)) -> case uri of
          Just ref | take 5 ref /= "data:" -> pure (ImageFile (takeDirectory path </> ref))
          _ -> do
            size <- fromIntegral <$> cImageBytes p (fromIntegral i) nullPtr
            unless (size > 0 && size <= 64 * 1024 * 1024) (ioError (userError "Unsupported or oversized embedded glTF image"))
            bytes <- allocaBytes size $ \out -> do
              copied <- cImageBytes p (fromIntegral i) out
              unless (fromIntegral copied == size) (ioError (userError "Invalid embedded glTF image"))
              B.packCStringLen (castPtr out, size)
            let imageMime = case uri of
                  Just ref | take 5 ref == "data:" -> takeWhile (/= ';') (drop 5 ref)
                  _ -> mime
            unless (imageMime `elem` ["image/png", "image/jpeg"]) (ioError (userError "Unsupported embedded glTF image MIME type"))
            pure (ImageBytes (if imageMime == "image/jpeg" then ".jpg" else ".png") bytes)
      )
  where
    parser = A.withObject "images" (\o -> o .:? "images" .!= ([] :: [A.Value]) >>= mapM (A.withObject "image" (\image -> (,) <$> image .:? "uri" <*> image .:? "mimeType" .!= ("image/png" :: String))))

imageSlots :: Int -> A.Value -> A.Parser (IM.IntMap [Maybe Int])
imageSlots imageCount = A.withObject "texture identities" $ \o -> do
  textures <- o .:? "textures" .!= ([] :: [A.Value])
  materials <- o .:? "materials" .!= ([] :: [A.Value])
  let resolve info = case info of
        Nothing -> pure Nothing
        Just value ->
          A.withObject
            "texture info"
            ( \t -> do
                index <- t .: "index"
                unless (index >= 0 && index < length textures) (fail "Invalid texture index")
                source <- A.withObject "texture" (.: "source") (textures !! index)
                unless (source >= 0 && source < imageCount) (fail "Invalid texture image")
                pure (Just source)
            )
            value
  slots <- forM materials $ A.withObject "material" $ \m -> do
    pbr <- m .:? "pbrMetallicRoughness" .!= A.Object KM.empty
    (base, packed) <- A.withObject "PBR" (\p -> (,) <$> p .:? "baseColorTexture" <*> p .:? "metallicRoughnessTexture") pbr
    normal <- m .:? "normalTexture"
    emission <- m .:? "emissiveTexture"
    occlusion <- m .:? "occlusionTexture"
    mapM resolve [base, normal, packed, packed, emission, occlusion]
  pure (IM.fromList (zip [0 ..] slots))
