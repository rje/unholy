-- Shared headless geometry decoding. No window, importer GPU objects or draws.
module Unholy.Asset.CPU (loadTriangles) where

import Control.Monad (unless)
import Data.Char (toLower)
import qualified Data.Vector.Storable as VS
import Linear (V3 (..))
import System.FilePath (takeExtension)
import qualified Unholy.Animation as Animation
import qualified Unholy.Asset.GLTF as Asset
import qualified Unholy.Renderer.Primitives as P
import Unholy.Types (defaultAnimator)

loadTriangles :: FilePath -> IO (VS.Vector (V3 Float))
loadTriangles path | Just cpu <- P.primitive path = do
  let raw = P.vertices cpu
      vertex i = V3 (raw VS.! (i * 8)) (raw VS.! (i * 8 + 1)) (raw VS.! (i * 8 + 2))
  pure (if VS.null (P.indices cpu) then VS.generate (VS.length raw `div` 8) vertex else VS.map (vertex . fromIntegral) (P.indices cpu))
loadTriangles path = do
  unless (map toLower (takeExtension path) `elem` [".gltf", ".glb"]) (ioError (userError ("CPU mesh colliders support glTF/GLB and built-in primitives: " ++ path)))
  rig <- Asset.loadAsset path
  unless (all ((== Nothing) . Asset.meshSkin) (Asset.rigMeshes rig)) (ioError (userError "Collider requires unskinned triangle geometry"))
  let palette = Animation.posePalette rig defaultAnimator
      triangles = VS.concat [let positions = Animation.posedPositions rig palette mesh in VS.map (\i -> positions VS.! fromIntegral i) (Asset.triangleIndices mesh) | mesh <- Asset.rigMeshes rig]
  unless (not (VS.null triangles) && VS.length triangles <= 1000000) (ioError (userError "Invalid or oversized collider geometry"))
  pure triangles
