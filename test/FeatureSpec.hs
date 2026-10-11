module FeatureSpec (prefabAssembly, physicsQueries, runtimeLifecycle, gizmoTools, marblePrefabs) where
import Unholy.Types
import Unholy.Transform
import Unholy.Prefab
import Unholy.Project
import Unholy.JSON (encodeJSON)
import Unholy.Physics
import Unholy.Script
import qualified Unholy.Gizmo as G
import qualified Unholy.Renderer.Camera as C
import qualified Linear as L
import qualified Data.IntMap.Strict as IM
import Control.Monad (unless,foldM)
import Control.Exception (finally)
import System.Directory (createDirectoryIfMissing)
import Data.List (isInfixOf)
assert :: String -> Bool -> IO ()
assert label okay=unless okay (ioError (userError label))
near :: V3 -> V3 -> Bool
near a b=all (<0.001) (zipWith (\x y->abs (x-y)) (vlist a) (vlist b))
prefabAssembly :: IO ()
prefabAssembly=do
  let (root,w0)=spawnMesh "Assembly" "cube" emptyWorld
      (child,w1)=spawnMesh "Trim" "sphere" w0
      pose=Transform (V3 4 2 3) (V3 0 30 0) (V3 2 2 2)
      local=identity {position=V3 1 0 0,scale=V3 0.2 0.2 0.2}
      w=resolveWorld w1 {transforms=IM.insert root pose (transforms w1),parents=IM.singleton child (Parent root local)}
  prefab<-either (ioError.userError) pure (makePrefab root w)
  let placement=identity {position=V3 10 0 0}
  (copy,ids,placed)<-either (ioError.userError) pure (instantiate prefab placement w)
  let trim=last ids;rootTr=transforms placed IM.! copy
  assert "prefab preserves root rotation/scale and local children" (scale rootTr==scale pose && near (position (transforms placed IM.! trim)) (position (compose rootTr local)))
  let moved=setTransform copy (rootTr {position=V3 12 1 0,rotation=V3 0 90 0}) placed
      (dupe,duplicated)=duplicate copy moved
      removed=destroy copy duplicated
  assert "children follow root edits" (near (position (transforms moved IM.! trim)) (V3 12 1 (-2)))
  assert "assembly duplication remaps parents" (length (descendants dupe duplicated)==2 && validateWorld duplicated==Right ())
  assert "recursive deletion only removes that instance" (IM.notMember trim (names removed) && length (descendants dupe removed)==2)
  createDirectoryIfMissing True "test-output"
  saved<-savePrefab "test-output/assembly.prefab.json" prefab
  assert "prefab saves" (saved==Right ())
  loaded<-loadPrefab "test-output" "assembly.prefab.json"
  assert "prefab disk round trip" (either (const False) ((==prefab).id) loaded)
  decoded<-either (ioError.userError) pure (decodeProject (encodeJSON (projectJSON ((emptyProject "Assembly") {world=moved}))))
  assert "project preserves parenting" (parents (world decoded)==parents moved && near (position (transforms (world decoded) IM.! trim)) (position (transforms moved IM.! trim)))
physicsQueries :: IO ()
physicsQueries=do
  let (sensor,w0)=spawn "Sensor" emptyWorld
      (ball,w1)=spawnMesh "Ball" "sphere" w0
      w=w1 {bodies=IM.fromList [(sensor,defaultBody {trigger=True}),(ball,defaultBody {mass=1,bodyShape=Sphere})]}
  phy<-createPhysics w
  (do
    configurePhysics phy (defaultPhysicsSettings {gravity=V3 0 0 0})
    entered<-stepPhysics phy (1/120) w
    events<-collisionEvents phy
    assert "overlap emits trigger enter without grounding" (any (\e->collisionKind e=="enter" && collisionTrigger e) events && not (grounded (bodies entered IM.! ball)))
    stayed<-stepPhysics phy (1/120) entered
    stay<-collisionEvents phy
    assert "persistent pair emits stay once" (map collisionKind stay==["stay"])
    hit<-raycastPhysics phy stayed (V3 0 3 0) (V3 0 (-3) 0) (Just sensor)
    assert "closest ray ignores selected entity and returns surface" (maybe False (\h->hitEntity h==ball && hitFraction h>0 && hitFraction h<1 && near (hitNormal h) (V3 0 1 0)) hit)
    let away=setTransform ball (identity {position=V3 4 0 0}) stayed
    exited<-stepPhysics phy (1/120) away
    exit<-collisionEvents phy
    assert "teleport breaks pair with exit" (map collisionKind exit==["exit"])
    synchronizePhysics phy (destroy ball exited)
    count<-bodyCount phy
    assert "destroy releases Bullet body immediately" (count==1)
    configurePhysics phy (defaultPhysicsSettings {gravity=V3 0 9 0})
    rising<-stepPhysics phy 0.1 away
    assert "Haskell project gravity controls solver" (let V3 _ y _=velocity (bodies rising IM.! ball) in y>0.8)
    ) `finally` closePhysics phy
  closePhysics phy
  -- A tipping box has angular motion even when its linear velocity is zero.
  -- Teleporting to its current pose must stop that motion too.
  let (floorId,t0)=spawn "Floor" emptyWorld
      (box,t1)=spawn "Tipping box" t0
      tipped=t1 {transforms=IM.fromList [(floorId,identity {position=V3 0 (-0.5) 0,scale=V3 10 1 10}),(box,identity {position=V3 0 1.2 0,rotation=V3 0 0 25,scale=V3 0.7 2 0.7})],bodies=IM.fromList [(floorId,defaultBody),(box,defaultBody {mass=1})]}
  solver<-createPhysics tipped
  (do
    touching<-foldM (\currentWorld _->stepPhysics solver (1/120) currentWorld) tipped [1..36::Int]
    configurePhysics solver (defaultPhysicsSettings {gravity=V3 0 0 0})
    let free=destroy floorId touching
    spinning<-stepPhysics solver 0.1 free
    assert "fixture has angular motion" (not (near (rotation (transforms free IM.! box)) (rotation (transforms spinning IM.! box))))
    requestTeleport solver box
    stopped<-stepPhysics solver 0.1 spinning {bodies=IM.adjust (\b->b {velocity=V3 0 0 0}) box (bodies spinning)}
    assert "same-pose teleport resets angular motion" (near (rotation (transforms spinning IM.! box)) (rotation (transforms stopped IM.! box)))
    ) `finally` closePhysics solver
runtimeLifecycle :: IO ()
runtimeLifecycle=do
  createDirectoryIfMissing True "test-output"
  writeFile "test-output/child.lua" $ unlines
    ["return {start=function(self) print('child started'); self.material.metallic=.8 end,"
    ,"on_trigger=function(self,event,scene) if event.kind=='enter' then assert(event.other.name=='Sensor'); print('collected'); scene:destroy(self) end end}"]
  writeFile "test-output/spawner.lua" $ unlines
    ["local child, ticks=nil,0; return {"
    ,"start=function(self,dt,input,scene) child=scene:spawn('child.prefab.json',{x=0,y=0,z=0}); assert(child.alive); scene.physics.gravity={x=0,y=0,z=0} end,"
    ,"update=function(self,dt,input,scene) ticks=ticks+1; if ticks==2 then assert(not child.alive); assert(scene:get(child.id)==nil); local hit=scene:raycast({x=0,y=3,z=0},{x=0,y=-3,z=0}); assert(hit.entity.name=='Sensor'); print('lifecycle okay') end end}"]
  let (root,pw)=spawnMesh "Collectible" "sphere" emptyWorld
      (trim,pw1)=spawnMesh "Child decoration" "cube" pw
      prefab=Prefab root (resolveWorld pw1 {bodies=IM.singleton root defaultBody {mass=1,bodyShape=Sphere},scripts=IM.singleton root "child.lua",parents=IM.singleton trim (Parent root (identity {scale=V3 0.1 0.1 0.1}))})
  saved<-savePrefab "test-output/child.prefab.json" prefab
  assert (show saved) (saved==Right ())
  let (sensor,w0)=spawn "Sensor" emptyWorld
      (spawner,w1)=spawn "Spawner" w0
      p=(emptyProject "Lifecycle") {world=w1 {bodies=IM.singleton sensor defaultBody {trigger=True},scripts=IM.singleton spawner "spawner.lua"}}
  (r,started,logs)<-startRuntime "test-output" p
  assert (show logs) (logs==["child started"] && IM.size (names (world started))==4)
  (r1,first,msg,_)<-stepRuntime (1/120) [] r started
  (r2,second,msg2,_)<-stepRuntime (1/120) [] r1 first
  stopRuntime r2
  assert (show (msg,msg2)) (msg==["collected"] && msg2==["lifecycle okay"] && IM.size (names (world second))==2)
  assert "destroyed scripts stay removed" (not (any (isInfixOf "Entity ") (msg++msg2)))

gizmoTools :: IO ()
gizmoTools=do
  let rect=(100,50,800,600);cam=Camera (V3 0 0 5) (V3 0 0 0) 45;pose=identity
      size=G.handleSize rect cam pose
      pixel p=maybe (L.V2 0 0) id (G.projectPoint rect cam p)
      center=pixel (L.V3 0 0 0);end=pixel (L.V3 size 0 0)
      mouse (L.V2 x y)=C.Mouse x y 0 0 0 False False True True
      halfway=(center+end)/2
      movedMouse=mouse (halfway+(end-center)/2)
      (drag,started,captured)=G.update rect cam (mouse halfway) True False (Just G.Move) (Just (1,pose)) Nothing
      (_,finished,_)=G.update rect cam movedMouse False True (Just G.Move) (Just (1,pose)) drag
  assert "axis handle captures without selecting geometry" (captured && maybe False G.editStarted started)
  assert "release emits one snapped move" (maybe False (\edit->G.editEnded edit && let V3 x y z=position (G.editTransform edit) in abs (x-0.25)<0.001 && y==0 && z==0) finished)
  let scaled=G.applyDrag rect cam (mouse (end+(end-center))) False (G.Drag 1 G.Scale 0 pose end)
      shrunk=G.applyDrag rect cam (mouse (center-10*(end-center))) False (G.Drag 1 G.Scale 0 pose end)
      rotated=G.applyDrag rect cam (mouse (pixel (L.V3 0 size 0))) True (G.Drag 1 G.Rotate 2 pose end)
      V3 _ _ angle=rotation rotated
  assert "scale handle affects its local axis and stays positive" (near (scale scaled) (V3 2 1 1) && all (>0) (vlist (scale shrunk)))
  assert "rotation ring produces world-axis quaternion rotation" (abs (angle-90)<0.001)
marblePrefabs :: IO ()
marblePrefabs=do
  p<-loadProject "projects/marble-run/project.hj.json" >>= either (ioError.userError) pure
  (r,started,_)<-startRuntime "projects/marble-run" p
  let w=world started;marble=1
      gem=fst (IM.findMin (IM.filter (=="Aether gem") (names w)))
      atGem=started {world=setTransform marble ((transforms w IM.! marble) {position=position (transforms w IM.! gem)}) w}
  (r1,collected,errors,_)<-stepRuntime (1/120) [] r atGem
  assert (show errors) (null errors && IM.notMember gem (names (world collected)))
  (r2,withCrate,errors2,_)<-stepRuntime (1/120) [0,0,0,0,1] r1 collected
  let crates=IM.filter (=="Physics crate") (names (world withCrate))
  assert "E spawns a scripted physics crate assembly" (null errors2 && IM.size crates==1 && all (\e->IM.member e (bodies (world withCrate)) && length (descendants e (world withCrate))==2) (IM.keys crates))
  (r3,reset,errors3,_)<-stepRuntime (1/120) [0,0,0,1,0] r2 withCrate
  stopRuntime r3
  assert "restart removes old props and recreates five gems" (null errors3 && IM.size (IM.filter (=="Aether gem") (names (world reset)))==5 && IM.null (IM.filter (=="Physics crate") (names (world reset))))
