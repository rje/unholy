-- Haskell samples validated rigs and uploads palettes; shaders deform vertices.
module Unholy.Animation
  ( Rig,
    RigMesh,
    loadRig,
    validateAnimator,
    advanceAnimators,
    sampleGlobals,
    rigMeshes,
    rigClips,
    bindVertices,
    triangleIndices,
    vertexJoints,
    vertexWeights,
    meshSkin,
    meshNode,
    meshMaterial,
    paletteOffset,
    posePalette,
    posedBounds,
    posedPositions,
    posedMeshes,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Fixed (mod')
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as VS
import Linear
import qualified Unholy.Asset.GLTF as Asset
import Unholy.Asset.GLTF.Internal
import Unholy.Types (Animator (..), World (..))

loadRig :: FilePath -> IO Rig
loadRig path = do
  rig <- Asset.loadAsset path
  forM_ (rigMaterialError rig) (ioError . userError)
  pure rig

v3At :: VS.Vector Float -> Int -> V3 Float
v3At a i = V3 (a VS.! i) (a VS.! (i + 1)) (a VS.! (i + 2))

quaternion :: V4 Float -> Quaternion Float
quaternion (V4 x y z w) = normalize (Quaternion w (V3 x y z))

trs :: V3 Float -> Quaternion Float -> V3 Float -> M44 Float
trs t q (V3 x y z) = mkTransformation q t !*! V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)

validateAnimator :: Rig -> Animator -> Either String ()
validateAnimator rig a
  | null (animationClip a) || M.member (animationClip a) (rigClips rig) = Right ()
  | otherwise = Left ("Unknown animation clip: " ++ animationClip a)

advanceAnimators :: Float -> World -> World
advanceAnimators dt w = w {animators = IM.map (\a -> if animationPlaying a then a {animationTime = animationTime a + dt * animationRate a} else a) (animators w)}

sampleGlobals :: Rig -> Animator -> V.Vector (M44 Float)
sampleGlobals rig a = runST $ do
  result <- MV.replicate (V.length locals) identity
  V.forM_ (rigOrder rig) $ \i -> do
    let parent = nodeParent (rigNodes rig V.! i)
    value <- if parent < 0 then pure (locals V.! i) else (!*! (locals V.! i)) <$> MV.read result parent
    MV.write result i value
  V.freeze result
  where
    clip = M.lookup (animationClip a) (rigClips rig)
    time = maybe 0 (\c -> if animationLoop a && clipDuration c > 0 then animationTime a `mod'` clipDuration c else min (animationTime a) (clipDuration c)) clip
    channels = maybe [] clipChannels clip
    changes = foldl' (\table c -> IM.insertWith (++) (channelNode c) [(channelPath c, sample time c)] table) IM.empty channels
    locals = V.imap local (rigNodes rig)
    local i n = case IM.lookup i changes of
      Nothing -> nodeMatrix n
      Just values ->
        let vec path fallback = maybe fallback (\(V4 x y z _) -> V3 x y z) (lookup path values)
            q = maybe (nodeRotation n) quaternion (lookup "rotation" values)
         in trs (vec "translation" (nodeTranslation n)) q (vec "scale" (nodeScale n))
    sample t c
      | VS.length (keyTimes c) == 1 || t <= VS.head (keyTimes c) = VS.head (keyValues c)
      | t >= VS.last (keyTimes c) = VS.last (keyValues c)
      | otherwise =
          let index = search 0 (VS.length (keyTimes c) - 1)
              start = keyTimes c VS.! index
              end = keyTimes c VS.! (index + 1)
              alpha = (t - start) / (end - start)
              left = keyValues c VS.! index
              right = keyValues c VS.! (index + 1)
           in if channelStep c then left else if channelPath c == "rotation" then let Quaternion w (V3 x y z) = slerp (quaternion left) (quaternion right) alpha in V4 x y z w else lerp alpha left right
      where
        search lo hi
          | hi - lo <= 1 = lo
          | keyTimes c VS.! mid <= t = search mid hi
          | otherwise = search lo mid
          where
            mid = (lo + hi) `div` 2

-- Each palette entry contains position and inverse-transpose normal matrices.
-- Nodes come first (rigid attachments), followed by each skin's joint palette.
posePalette :: Rig -> Animator -> VS.Vector (M44 Float)
posePalette rig a = VS.fromList (concatMap (\m -> [m, transpose (inv44 m)]) matrices)
  where
    globals = sampleGlobals rig a
    matrices =
      V.toList globals
        ++ concat
          [zipWith (\joint bind -> globals V.! joint !*! bind) (skinJoints skin) (inverseBinds skin) | skin <- V.toList (rigSkins rig)]

paletteOffset :: Rig -> RigMesh -> Int
paletteOffset rig mesh = case meshSkin mesh of
  Nothing -> meshNode mesh
  Just index -> V.length (rigNodes rig) + sum [length (skinJoints skin) | skin <- V.toList (V.take index (rigSkins rig))]

posedBounds :: Rig -> VS.Vector (M44 Float) -> RigMesh -> Bounds
posedBounds rig palette mesh =
  foldl1
    combine
    [transformBounds' (palette VS.! (2 * (offset + joint))) bounds | (joint, bounds) <- IM.toList (influenceBounds mesh)]
  where
    offset = paletteOffset rig mesh
    combine (a, b) (c, d) = (liftI2 min a c, liftI2 max b d)
    transformBounds' matrix (V3 ax ay az, V3 bx by bz) =
      let points = [xyz (matrix !* V4 x y z 1) | x <- [ax, bx], y <- [ay, by], z <- [az, bz]]
       in (foldl1 (liftI2 min) points, foldl1 (liftI2 max) points)

-- Evaluated only after a picking bounds hit or by the CPU reference tests.
posedPositions :: Rig -> VS.Vector (M44 Float) -> RigMesh -> VS.Vector (V3 Float)
posedPositions rig palette mesh = VS.generate (VS.length (bindVertices mesh) `div` 8) $ \i ->
  weighted rig palette mesh i 0 (v3At (bindVertices mesh) (i * 8)) 1

weighted :: Rig -> VS.Vector (M44 Float) -> RigMesh -> Int -> Int -> V3 Float -> Float -> V3 Float
weighted rig palette mesh i normal (V3 x y z) w =
  foldl'
    (^+^)
    (V3 0 0 0)
    [weight *^ xyz (palette VS.! (2 * (offset + fromIntegral joint) + normal) !* V4 x y z w) | (joint, weight) <- zip (toList4 (vertexJoints mesh VS.! i)) (toList4 (vertexWeights mesh VS.! i)), weight > 0]
  where
    offset = paletteOffset rig mesh

xyz :: V4 Float -> V3 Float
xyz (V4 x y z _) = V3 x y z

toList4 :: V4 a -> [a]
toList4 (V4 x y z w) = [x, y, z, w]

-- CPU reference kept for parity checks; the renderer never calls this per frame.
posedMeshes :: Rig -> Animator -> [VS.Vector Float]
posedMeshes rig a = map deform (rigMeshes rig)
  where
    palette = posePalette rig a
    deform mesh = VS.generate (VS.length (bindVertices mesh)) $ \index ->
      let (i, c) = index `divMod` 8
          V3 x y z = weighted rig palette mesh i 0 (v3At (bindVertices mesh) (i * 8)) 1
          V3 nx ny nz = normalize (weighted rig palette mesh i 1 (v3At (bindVertices mesh) (i * 8 + 5)) 0)
       in if c < 3 then [x, y, z] !! c else if c < 5 then bindVertices mesh VS.! index else [nx, ny, nz] !! (c - 5)
