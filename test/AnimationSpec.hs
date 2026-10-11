module AnimationSpec (animationContracts) where

import Unholy.Types
import Unholy.Animation
import Unholy.Project
import qualified Unholy.JSON
import Unholy.Prefab
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Control.Monad (unless)
import Control.Exception (try,IOException)
import System.Directory (createDirectoryIfMissing,copyFile)

assert :: String -> Bool -> IO ()
assert label okay=unless okay (ioError (userError label))
animationContracts :: IO ()
animationContracts=do
  rig<-loadRig "test/fixtures/animation/fixture.gltf"
  let config clip time loop=defaultAnimator {animationClip=clip,animationTime=time,animationLoop=loop}
      vertices a=head (posedMeshes rig a)
      close x y=abs (x-y)<0.0001
      firstX a=vertices a VS.! 0
  assert "Bind pose applied skin mesh node transform or lost inverse binds" (close (firstX defaultAnimator) (-1) && close (vertices defaultAnimator VS.! 1) 0)
  assert "Non-joint hierarchy or two-weight blend incorrect" (close (firstX (config "move" 0.5 False)) (-0.5))
  assert "Animation clamps and loops incorrectly" (close (firstX (config "move" 1 False)) 0 && close (firstX (config "move" 1.25 True)) (-0.75))
  assert "STEP exact boundary or single key failed" (close (firstX (config "step" 0.499 False)) (-1) && close (firstX (config "step" 0.5 False)) 0 && close (firstX (config "single" 3 True)) 0)
  assert "Quaternion interpolation incorrect" (close (firstX (config "turn" 0.5 False)) 0 && close (vertices (config "turn" 0.5 False) VS.! 9) 1)
  let shortest=vertices (config "shortest" 0.5 False)
      expected=(-1+(0))*0.5
  assert "Antipodal quaternion took the long path" (abs (shortest VS.! 0-expected)<0.001)
  let (e,base)=spawnMesh "Rig" "fixture.gltf" emptyWorld
      saved=config "move" 0.25 False
      w=base {animators=IM.singleton e saved}
      (_,duplicateWorld)=duplicate e w
      advanced=advanceAnimators 0.5 w
      paused=advanceAnimators 0.5 w {animators=IM.singleton e saved {animationPlaying=False}}
  assert "Animator clock/pause failed" (animationTime (animators advanced IM.! e)==0.75 && animationTime (animators paused IM.! e)==animationTime saved)
  assert "Animator clone/delete failed" (IM.size (animators duplicateWorld)==2 && IM.null (animators (destroy e w)))
  prefab<-either (ioError.userError) pure (makePrefab e w)
  (_,_,placed)<-either (ioError.userError) pure (instantiate prefab identity emptyWorld)
  assert "Animator prefab lost configuration" (IM.elems (animators placed)==[saved])
  let project=(emptyProject "Animation") {world=w}
  assert "Animator persistence failed" (decodeProject (encodeProject project)==Right project)
  missing<-checkAssets "test/fixtures/animation" project
  assert ("Rig asset preflight failed: "++show missing) (null missing)
  invalid<-checkAssets "test/fixtures/animation" project {world=w {animators=IM.singleton e saved {animationClip="missing"}}}
  assert "Invalid clip was accepted" (not (null invalid))
  let (_,mixed)=spawnMesh "Static reuse" "fixture.gltf" w
      mixedWorld=mixed {meshes=IM.adjust (\m->m {useImportedMaterials=True}) 2 (meshes mixed)}
  mixedErrors<-checkAssets "test/fixtures/animation" project {world=mixedWorld}
  assert "A static user bypassed skin policy through animated reuse" (not (null mixedErrors))
  createDirectoryIfMissing True "test-output/animation"
  copyFile "test/fixtures/animation/fixture.bin" "test-output/animation/fixture.bin"
  source<-readFile "test/fixtures/animation/fixture.gltf"
  writeFile "test-output/animation/invalid.gltf" (replace "LINEAR" "CUBICSPLINE" source)
  rejected<-try (loadRig "test-output/animation/invalid.gltf") :: IO (Either IOException Rig)
  assert "Unsupported interpolation was accepted" (either (const True) (const False) rejected)
  writeFile "test-output/animation/missing-base.gltf" (replace "\"bufferView\":0," "" source)
  noBase<-try (loadRig "test-output/animation/missing-base.gltf") :: IO (Either IOException Rig)
  assert "Unsafe missing-base geometry reached native importer" (either (const True) (const False) noBase)
  where
    encodeProject=Unholy.JSON.encodeJSON . projectJSON
    replace _ _ []=[]
    replace needle value input | take (length needle) input==needle=value++replace needle value (drop (length needle) input)
                              | otherwise=head input:replace needle value (tail input)
