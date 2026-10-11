module PhysicsSpec (extendedColliders,nodeFixture) where

import Unholy.Types
import Unholy.Physics
import Unholy.Asset.CPU
import qualified Unholy.Renderer.Camera as C
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import qualified Linear as L
import Control.Exception (try,IOException,finally)
import Control.Monad (unless,foldM)
import System.Directory (createDirectoryIfMissing)
import System.IO (withBinaryFile,IOMode(WriteMode),hPutBuf)

assert :: String -> Bool -> IO ()
assert label okay=unless okay (ioError (userError label))

extendedColliders :: IO ()
extendedColliders=do
  let (_,w0)=spawnMesh "Ground" "plane" emptyWorld
      (_,w1)=spawnMesh "Capsule" "capsule" w0
      (_,w2)=spawnMesh "Convex crate" "cube" w1
      w=w2 {transforms=IM.fromList [(1,identity {scale=V3 20 1 20}),(2,identity {position=V3 0 4 0}),(3,identity {position=V3 3 3 0,rotation=V3 0 0 15})]
        ,bodies=IM.fromList [(1,defaultBody {bodyShape=TriangleMesh}),(2,defaultBody {bodyShape=Capsule,mass=1,lockRotation=True}),(3,defaultBody {bodyShape=Convex,mass=1})]}
  physics<-createPhysics w
  final<-foldM (\current _->stepPhysics physics (1/120) current) w [1..300::Int]
  let V3 _ y _=position (transforms final IM.! 2)
      V3 _ crateY _=position (transforms final IM.! 3)
  assert "Capsule height/radius or upright lock is wrong" (abs (y-1)<0.06 && rotation (transforms final IM.! 2)==V3 0 0 0 && grounded (bodies final IM.! 2))
  assert "Convex hull failed to settle on triangle surface" (crateY>0.45 && crateY<0.65)
  requestTeleport physics 2
  let failed=final {bodies=IM.adjust (\b->b {friction=0.3}) 2 (bodies final),meshes=IM.adjust (\m->m {geometry="missing.gltf"}) 3 (meshes final)}
  outcome<-try (synchronizePhysics physics failed) :: IO (Either IOException ())
  count<-bodyCount physics
  assert "Failed later collider did not preserve old registry" (either (const True) (const False) outcome && count==3)
  _<-stepPhysics physics (1/120) final
  hit<-raycastPhysics physics final (V3 8 5 0) (V3 8 (-1) 0) Nothing
  assert "Old world unusable after collider failure" (maybe False ((==1).hitEntity) hit)
  closePhysics physics
  closePhysics physics
  initialFailure<-try (createPhysics failed) :: IO (Either IOException Physics)
  assert "Invalid initial world did not fail" (either (const True) (const False) initialFailure)
  fresh<-createPhysics w
  closePhysics fresh
  let wrong=w {transforms=IM.adjust (\t->t {scale=V3 1 2 1}) 2 (transforms w)}
  assert "Nonuniform capsule should fail validation" (validateWorld wrong/=Right ())
  assert "Moving triangle collider should fail validation" (validateWorld w {bodies=IM.adjust (\b->b {kinematic=True}) 1 (bodies w)}/=Right ())
  importedCollider

-- Two mesh nodes beneath a translated/rotated/scaled parent, then an outer
-- entity pose. Collision hits must agree with the transformed CPU triangles.
nodeFixture :: IO (FilePath,String)
nodeFixture=do
  let root="test-output/colliders"
      path=root++"/nodes.gltf"
      raw=VS.fromList [-1,0,-1,-1,0,1,1,0,1] :: VS.Vector Float
      json="{\"asset\":{\"version\":\"2.0\"},\"buffers\":[{\"uri\":\"nodes.bin\",\"byteLength\":36}],\"bufferViews\":[{\"buffer\":0,\"byteLength\":36}],\"accessors\":[{\"bufferView\":0,\"componentType\":5126,\"count\":3,\"type\":\"VEC3\"}],\"meshes\":[{\"primitives\":[{\"attributes\":{\"POSITION\":0}}]}],\"nodes\":[{\"translation\":[2,1,0],\"rotation\":[0,0.70710678,0,0.70710678],\"scale\":[2,1,3],\"children\":[1,2]},{\"mesh\":0,\"translation\":[0,1,0]},{\"mesh\":0,\"translation\":[0,3,0]}],\"scenes\":[{\"nodes\":[0]}],\"scene\":0}"
  createDirectoryIfMissing True root
  VS.unsafeWith raw $ \p->withBinaryFile (root++"/nodes.bin") WriteMode $ \file->hPutBuf file p 36
  writeFile path json
  pure (root,json)

importedCollider :: IO ()
importedCollider=do
  (root,json)<-nodeFixture
  positions<-loadTriangles (root++"/nodes.gltf")
  assert "Node instances were not both decoded" (VS.length positions==6)
  let outer=identity {position=V3 0 0.5 0,scale=V3 0.5 1 0.5}
      (e,base)=spawnMesh "Imported surface" "nodes.gltf" emptyWorld
      w=base {transforms=IM.singleton e outer,bodies=IM.singleton e defaultBody {bodyShape=TriangleMesh}}
      matrix=C.modelMatrix outer L.identity
      ray=C.Ray (L.V3 1.5 8 (1/3)) (L.V3 0 (-1) 0)
      picked=C.rayMesh (C.objectRay matrix ray) (C.boundsOf positions) positions VS.empty
  physics<-createPhysicsAt root w
  hit<-raycastPhysics physics w (V3 1.5 8 (1/3)) (V3 1.5 0 (1/3)) Nothing `finally` closePhysics physics
  assert ("Bullet/picking node transforms disagree: "++show (picked,hit)) (maybe False (\h->abs (hitFraction h*8-maybe (-1) id picked)<0.001) hit)
  -- A malformed VEC2 POSITION must be rejected before a C transform reads z.
  writeFile (root++"/bad.gltf") (replaceVec3 json)
  invalid<-try (loadTriangles (root++"/bad.gltf")) :: IO (Either IOException (VS.Vector (L.V3 Float)))
  assert "Malformed POSITION layout was accepted" (either (const True) (const False) invalid)
  where replaceVec3 []=[]
        replaceVec3 ('V':'E':'C':'3':rest)="VEC2"++rest
        replaceVec3 (c:rest)=c:replaceVec3 rest
