module AnimationSpec (animationContracts) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Linear (V4 (..), norm)
import qualified Linear as L
import System.Directory (copyFile, createDirectoryIfMissing)
import Unholy.Animation
import qualified Unholy.Asset.GLTF as Asset
import qualified Unholy.JSON
import Unholy.Prefab
import Unholy.Project
import Unholy.Types

assert :: String -> Bool -> IO ()
assert label okay = unless okay (ioError (userError label))

animationContracts :: IO ()
animationContracts = do
  rig <- loadRig "test/fixtures/animation/fixture.gltf"
  let config clip time loop = defaultAnimator {animationClip = clip, animationTime = time, animationLoop = loop}
      vertices a = head (posedMeshes rig a)
      close x y = abs (x - y) < 0.0001
      firstX a = vertices a VS.! 0
  assert "Bind pose applied skin mesh node transform or lost inverse binds" (close (firstX defaultAnimator) (-1) && close (vertices defaultAnimator VS.! 1) 0)
  assert "Non-joint hierarchy or two-weight blend incorrect" (close (firstX (config "move" 0.5 False)) (-0.5))
  assert "Animation clamps and loops incorrectly" (close (firstX (config "move" 1 False)) 0 && close (firstX (config "move" 1.25 True)) (-0.75))
  assert "STEP exact boundary or single key failed" (close (firstX (config "step" 0.499 False)) (-1) && close (firstX (config "step" 0.5 False)) 0 && close (firstX (config "single" 3 True)) 0)
  assert "Quaternion interpolation incorrect" (close (firstX (config "turn" 0.5 False)) 0 && close (vertices (config "turn" 0.5 False) VS.! 9) 1)
  let shortest = vertices (config "shortest" 0.5 False)
      expected = (-1 + (0)) * 0.5
  assert "Antipodal quaternion took the long path" (abs (shortest VS.! 0 - expected) < 0.001)
  let (e, base) = spawnMesh "Rig" "fixture.gltf" emptyWorld
      saved = config "move" 0.25 False
      w = base {animators = IM.singleton e saved}
      (_, duplicateWorld) = duplicate e w
      advanced = advanceAnimators 0.5 w
      paused = advanceAnimators 0.5 w {animators = IM.singleton e saved {animationPlaying = False}}
  assert "Animator clock/pause failed" (animationTime (animators advanced IM.! e) == 0.75 && animationTime (animators paused IM.! e) == animationTime saved)
  let fade = crossfade 1 "single" (config "move" 0 False)
      halfway = fade {animationBlend = fmap (\b -> b {blendElapsed = 0.5}) (animationBlend fade)}
      endpoint = fade {animationBlend = fmap (\b -> b {blendElapsed = 1}) (animationBlend fade)}
  assert "Crossfade local translation endpoints/intermediate pose" (close (firstX fade) (-1) && close (firstX halfway) (-0.5) && close (firstX endpoint) 0)
  assert "Repeated clip requests restarted the blend" (crossfade 1 "single" halfway == halfway)
  assert "Paused animator advanced its blend" (animators (advanceAnimators 0.5 w {animators = IM.singleton e halfway {animationPlaying = False}}) IM.! e == halfway {animationPlaying = False})
  assert "Completed fade retained its source pose" (animationBlend (animators (advanceAnimators 1 w {animators = IM.singleton e fade}) IM.! e) == Nothing)
  assert "Blend persistence failed" (decodeProject (encodeProject (emptyProject "Blend") {world = w {animators = IM.singleton e halfway}}) == Right (emptyProject "Blend") {world = w {animators = IM.singleton e halfway}})
  assert "Animator clone/delete failed" (IM.size (animators duplicateWorld) == 2 && IM.null (animators (destroy e w)))
  prefab <- either (ioError . userError) pure (makePrefab e w)
  (_, _, placed) <- either (ioError . userError) pure (instantiate prefab identity emptyWorld)
  assert "Animator prefab lost configuration" (IM.elems (animators placed) == [saved])
  let project = (emptyProject "Animation") {world = w}
  assert "Animator persistence failed" (decodeProject (encodeProject project) == Right project)
  missing <- checkAssets "test/fixtures/animation" project
  assert ("Rig asset preflight failed: " ++ show missing) (null missing)
  invalid <- checkAssets "test/fixtures/animation" project {world = w {animators = IM.singleton e saved {animationClip = "missing"}}}
  assert "Invalid clip was accepted" (not (null invalid))
  let (_, mixed) = spawnMesh "Static reuse" "fixture.gltf" w
      mixedWorld = mixed {meshes = IM.adjust (\m -> m {useImportedMaterials = True}) 2 (meshes mixed)}
  mixedErrors <- checkAssets "test/fixtures/animation" project {world = mixedWorld}
  assert "A static user bypassed skin policy through animated reuse" (not (null mixedErrors))
  createDirectoryIfMissing True "test-output/animation"
  copyFile "test/fixtures/animation/fixture.bin" "test-output/animation/fixture.bin"
  source <- readFile "test/fixtures/animation/fixture.gltf"
  writeFile "test-output/animation/invalid.gltf" (replace "LINEAR" "CUBICSPLINE" source)
  rejected <- try (loadRig "test-output/animation/invalid.gltf") :: IO (Either IOException Rig)
  assert "Unsupported interpolation was accepted" (either (const True) (const False) rejected)
  writeFile "test-output/animation/missing-base.gltf" (replace "\"bufferView\":0," "" source)
  noBase <- try (loadRig "test-output/animation/missing-base.gltf") :: IO (Either IOException Rig)
  assert "Legal implicit-zero accessor rejected by unified CPU importer" (either (const False) (const True) noBase)
  imported <- loadRig "test/fixtures/gltf/external.gltf"
  let [tag, skin] = rigMeshes imported
      raw = bindVertices skin
      normalAt n = VS.toList (VS.slice (n * 8 + 5) 3 raw)
      pose = posePalette imported (config "move" 0.5 False)
      closePoint a b = norm (a - b) < 0.0001
  assert "Missing normals must split shared corners into flat faces" (VS.length raw == 48 && all ((== [0, 0, 1]) . normalAt) [0 .. 2] && all ((== [0, 1, 0]) . normalAt) [3 .. 5])
  assert "Normalized UV/joint/weight data lost during corner expansion" (raw VS.! 28 == 0 && raw VS.! 36 == 1 && VS.all (== V4 0 1 0 0) (vertexJoints skin) && VS.all (\(V4 a b c d) -> close a (128 / 255) && close b (127 / 255) && c == 0 && d == 0) (vertexWeights skin))
  assert "Selected scene or rigid joint attachment is incorrect" (length (rigMeshes imported) == 2 && meshSkin tag == Nothing && closePoint (VS.head (posedPositions imported pose tag)) (L.V3 1 2 0) && closePoint (VS.head (posedPositions imported pose skin)) (L.V3 (127 / 255) 0 0))
  imageBytes <- B.readFile "test/fixtures/gltf/pixel.png"
  embedded <- mapM Asset.loadAsset ["test/fixtures/gltf/data-uri.gltf", "test/fixtures/gltf/embedded.glb"]
  assert "Data-URI/GLB images must preserve the same encoded bytes as external images" (all (\asset -> case V.toList (Asset.rigImages asset) of [Asset.ImageBytes ".png" bytes] -> bytes == imageBytes; _ -> False) embedded)
  where
    encodeProject = Unholy.JSON.encodeJSON . projectJSON
    replace _ _ [] = []
    replace needle value input
      | take (length needle) input == needle = value ++ replace needle value (drop (length needle) input)
      | otherwise = head input : replace needle value (tail input)
