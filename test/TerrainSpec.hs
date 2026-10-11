module TerrainSpec (terrainContracts) where

import Unholy
import Unholy.Project
import Unholy.JSON (encodeJSON)
import Unholy.Prefab
import qualified Unholy.Renderer.Primitives as P
import qualified Unholy.Renderer.Camera as C
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import qualified Linear as L
import Control.Monad (unless,forM_,foldM)
import Control.Exception (finally,try,IOException)
import System.Directory (createDirectoryIfMissing)

assert :: String -> Bool -> IO ()
assert label okay=unless okay (ioError (userError label))
terrainContracts :: IO ()
terrainContracts=do
  -- Nonplanar, positive-only heights expose both diagonal and midpoint mistakes.
  let terrain=Terrain 2 2 2 [1,2,3,7]
      (e,base)=spawnTerrain "Asymmetric heightfield" terrain emptyWorld
      outer=identity {position=V3 2 3 (-1),rotation=V3 0 30 0,scale=V3 1.5 2 0.7}
      w=base {transforms=IM.singleton e outer}
  physics<-createPhysics w
  (do
    agreement physics w terrain outer
    stepped<-stepPhysics physics (1/120) w
    assert "Height midpoint leaked into entity readback" (position (transforms stepped IM.! e)==position outer)
    let edited=setHeight 3 4 terrain;next=w {terrains=IM.singleton e edited}
    synchronizePhysics physics next
    agreement physics next edited outer
    let invalid=next {terrains=IM.singleton e edited {terrainHeights=[0/0,2,3,4]}}
    failure<-try (synchronizePhysics physics invalid) :: IO (Either IOException ())
    assert "Invalid terrain replacement was accepted" (either (const True) (const False) failure)
    agreement physics next edited outer) `finally` closePhysics physics
  let (copy,duplicated)=duplicate e w
      prefabResult=makePrefab e w >>= \prefab->instantiate prefab identity emptyWorld
  assert "Terrain duplicate/delete failed" (IM.lookup copy (terrains duplicated)==Just terrain && IM.null (terrains (destroy e w)))
  (_,_,placed)<-either (ioError.userError) pure prefabResult
  assert "Terrain prefab lost samples" (IM.elems (terrains placed)==[terrain])
  let project=(emptyProject "Finite terrain") {world=w}
  assert "Terrain JSON round trip failed" (decodeProject (encodeJSON (projectJSON project))==Right project)
  createDirectoryIfMissing True "test-output/terrain"
  saved<-saveProject "test-output/terrain/project.hj.json" project
  assert "Terrain save failed" (saved==Right ())
  reopened<-loadProject "test-output/terrain/project.hj.json"
  assert "Terrain reopened incorrectly" (reopened==Right project)
  assert "Oversized terrain accepted" (not (validTerrain (generateTerrain 130 2 1 (\_ _->0))))
  -- A real dynamic capsule crosses a nonflat part of the same finite samples.
  let hill=generateTerrain 17 17 0.5 (\_ z->0.2*sin (z*0.6))
      (_,floorWorld)=spawnTerrain "Rolling ground" hill emptyWorld
      (_,two)=spawnMesh "Traveler" "capsule" floorWorld
      moving=two {transforms=IM.insert 2 (identity {position=V3 0 2 3}) (transforms two),bodies=IM.insert 2 defaultBody {bodyShape=Capsule,mass=1,lockRotation=True} (bodies two)}
  p<-createPhysics moving
  final<-foldM (\current _->stepPhysics p (1/120) current {bodies=IM.adjust (\b->let V3 _ vy _=velocity b in b {velocity=V3 0 vy (-1)}) 2 (bodies current)}) moving [1..600::Int] `finally` closePhysics p
  let V3 _ y z=position (transforms final IM.! 2)
  assert "Capsule failed to traverse finite terrain" (z<(-1) && y>0.65 && y<1.1 && grounded (bodies final IM.! 2))
  where
    agreement physics w terrain outer=do
      cpu<-either (ioError.userError) pure (terrainMesh terrain)
      let raw=P.vertices cpu
          points=VS.generate (VS.length raw `div` 8) (\i->L.V3 (raw VS.! (i*8)) (raw VS.! (i*8+1)) (raw VS.! (i*8+2)))
          matrix=C.modelMatrix outer L.identity
      forM_ [(-0.6,0.5),(0.6,-0.5),(-0.5,-0.1),(0.1,0.5)] $ \(x,z)->do
        let L.V3 px _ pz=C.transformPoint matrix (L.V3 x 0 z)
            ray=C.Ray (L.V3 px 30 pz) (L.V3 0 (-1) 0)
            visual=C.rayMesh (C.objectRay matrix ray) (C.boundsOf points) points (P.indices cpu)
        hit<-raycastPhysics physics w (V3 px 30 pz) (V3 px (-5) pz) Nothing
        assert ("Terrain render/collision disagree: "++show (visual,hit)) (maybe False (\h->abs (hitFraction h*35-maybe (-100) id visual)<0.001) hit)
