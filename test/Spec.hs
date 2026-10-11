module Main where

import LibrarySpec
import RuntimeSpec
import FeatureSpec
import Unholy.Types
import Unholy.Project
import Unholy.JSON (encodeJSON)
import Unholy.Script
import Unholy.Physics
import qualified Data.IntMap.Strict as IM
import Control.Monad (unless,foldM)
import System.Directory
import System.FilePath
import Data.List (isInfixOf)
import Control.Exception (finally)
import qualified Unholy.Renderer.Camera as C
import qualified Linear as L
import qualified Data.Vector.Storable as VS

assert :: String -> Bool -> IO ()
assert label good = unless good (ioError (userError label))
test :: String -> IO () -> IO ()
test label action = action >> putStrLn ("PASS "++label)
load :: String -> IO Project
load name = loadProject ("projects" </> name </> projectFile) >>= either (ioError . userError) pure

main :: IO ()
main = do
  test "Haskell primitive CPU geometry, winding, UVs and collider dimensions" primitiveGeometry
  test "Typed game-owned components and Haskell input policy" typedInputComponents
  test "renderer camera rays, transformed picking and parallel bounds" $ do
    let cam=Camera (V3 0 0 5) (V3 0 0 0) 45
        mouse=C.Mouse 500 350 0 0 0 False False True True
        ray=C.screenRay (100,50,800,600) cam mouse
        tr=Transform (V3 0 0 0) (V3 0 90 0) (V3 2 1 0.5)
        model=C.modelMatrix tr L.identity
        triangle=VS.fromList [L.V3 (-1) (-1) 0,L.V3 1 (-1) 0,L.V3 0 1 0]
        plane=Transform (V3 0 0 1) (V3 0 0 30) (V3 2 1 0.5)
        transformed=C.objectRay (C.modelMatrix plane L.identity) ray
        nearHit=C.rayMesh transformed (C.boundsOf triangle) triangle VS.empty
        moved=C.transformPoint model (L.V3 1 0 0)
    assert "center pixel points at camera target" (L.norm (C.rayDirection ray-L.V3 0 0 (-1))<0.0001)
    assert "rotation/scale order preserves world geometry" (L.norm (moved-L.V3 0 0 (-2))<0.0001)
    assert "picking retains world distance under rotated nonuniform scale" (maybe False (\t->abs (t-4)<0.0001) nearHit)
    assert "parallel ray outside bounds misses" (not (C.rayBounds (C.Ray (L.V3 2 0 5) (L.V3 0 0 (-1))) (L.V3 (-1) (-1) (-1),L.V3 1 1 1)))
    assert "parallel ray on boundary intersects" (C.rayBounds (C.Ray (L.V3 1 0 5) (L.V3 0 0 (-1))) (L.V3 (-1) (-1) (-1),L.V3 1 1 1))
  test "typed ECS joins, duplication and deletion" $ do
    let (e,w)=spawnMesh "Sphere" "sphere" emptyWorld
        withBody=w {bodies=IM.insert e defaultBody (bodies w),scripts=IM.insert e "assets/test.lua" (scripts w)}
        (copy,w2)=duplicate e withBody
        w3=destroy e w2
    assert "IDs are unique" (copy>e && nextId w3>copy)
    assert "duplicate copies all components" (IM.member copy (bodies w3) && IM.member copy (scripts w3))
    assert "delete removes component data" (not (IM.member e (transforms w3)) && not (IM.member e (bodies w3)))
    assert "world invariant" (validateWorld w3==Right ())
  test "all sample projects validate and round-trip JSON" $ do
    mapM_ (\n->do
      p<-load n
      missing<-checkAssets ("projects" </> n) p
      assert (n++": missing assets") (null missing)
      assert (n++": JSON round trip") (decodeProject (encodeJSON (projectJSON p))==Right p)) ["material-lab","marble-run","knockdown"]
    assert "reject schema mismatch" (either (const True) (const False) (decodeProject "{\"schemaVersion\":9}"))
    assert "reject outside asset paths" (assetPath "../private.lua"/=Right ())
  test "Move, rotate and scale gizmos: capture, release, snapping and bounds" gizmoTools
  test "Marble prefab pickups, crate spawning and restart" marblePrefabs
  test "Prefab assemblies: transforms, duplication, deletion and persistence" prefabAssembly
  test "Physics triggers, raycasts, gravity and body lifetime" physicsQueries
  test "Spawned scripts start once and destroyed assemblies clean up" runtimeLifecycle
  test "Bullet settles a falling sphere and reports grounding" $ do
    let (floorId,w0)=spawnMesh "Floor" "cube" emptyWorld
        (ball,w1)=spawnMesh "Ball" "sphere" w0
        w=w1 {transforms=IM.fromList [(floorId,identity {position=V3 0 (-0.5) 0,scale=V3 10 1 10}),(ball,identity {position=V3 0 4 0})],bodies=IM.fromList [(floorId,defaultBody),(ball,defaultBody {bodyShape=Sphere,mass=1})]}
    physics<-createPhysics w
    final<-foldM (\current _->stepPhysics physics (1/120) current) w [1..360::Int] `finally` closePhysics physics
    let V3 _ y _=position (transforms final IM.! ball)
    assert ("sphere floor height: "++show y) (abs (y-0.5)<0.06)
    assert "sphere grounded" (grounded (bodies final IM.! ball))
  test "Lua mutates ECS data, another entity and camera; errors are contained" $ do
    createDirectoryIfMissing True "test-output"
    writeFile "test-output/control.lua" "return {update=function(self,dt,input,scene) self.position.x=self.position.x+dt*2; scene:get('Target').material.metallic=0.9; scene.camera.eye.x=12; end}"
    let (e,w0)=spawnMesh "Runner" "sphere" emptyWorld
        (other,w1)=spawnMesh "Target" "cube" w0
        p=(emptyProject "Lua fixture") {world=w1 {scripts=IM.singleton e "control.lua"}}
    (r,started,msg)<-startRuntime "test-output" p
    assert (show msg) (null msg)
    (r',next,errors,_)<-stepRuntime 0.25 [0,0,0,0] r started
    stopRuntime r'
    let V3 x _ _=position (transforms (world next) IM.! e)
        V3 cx _ _=eye (camera next)
    assert "Lua self position" (abs (x-0.5)<0.001)
    assert "Lua other entity" (abs (metallic (materials (world next) IM.! other)-0.9)<0.001)
    assert "Lua camera" (cx==12 && null errors)
    writeFile "test-output/control.lua" "return { update=function() while true do end end }"
    (badR,badP,_)<-startRuntime "test-output" p
    (stopped,_,failures,_)<-stepRuntime 0.01 [0,0,0,0] badR badP
    stopRuntime stopped
    assert "instruction limit" (any (isInfixOf "instruction budget") failures)
    writeFile "test-output/control.lua" "this is not valid lua"
    (syntaxR,_,syntaxErrors)<-startRuntime "test-output" p
    stopRuntime syntaxR
    assert "syntax errors reported" (not (null syntaxErrors))
  test "Lua live entity access, retained references, validation and rollback" luaAccess
  test "Haskell audio cache, overlapping voices, pause/mute and resource lifetime" audioPolicy
  test "Marble Run: controls, jump, checkpoint, finish, restart" marbleGame
  test "Marble Run: kinematic lift carries the marble and bridge assembly moves" marbleLift
  test "Knockdown: real Bullet collision topples a target" knockdownGame
  putStrLn "All engine integration checks passed."

marbleGame :: IO ()
marbleGame = do
  p<-load "marble-run"
  (r,started,msg)<-startRuntime "projects/marble-run" p
  assert (show msg) (not (any (isInfixOf "error") msg))
  let marble=fst (IM.findMin (scripts (world p)))
      control current =
        let w=world current
            V3 x _ z=position (transforms w IM.! marble)
            V3 vx _ vz=velocity (bodies w IM.! marble)
            checked=any ((>1) . emission) [m | (e,m)<-IM.toList (materials w),IM.lookup e (names w)==Just "Checkpoint beacon"]
            goalX=if checked then 9 else 0
            axis delta v = clamp ((clampSpeed (delta*3)-v)*0.8)
            jump=if z<2.35 && z>1.65 then 1 else 0
        in [axis (goalX-x) vx,axis (-5-z) vz,jump,0]
      loop count runtime current status
        | "COURSE COMPLETE" `isInfixOf` status=pure (runtime,current,status)
        | count>=2400=pure (runtime,current,status)
        | otherwise=do
          (nextR,nextP,errors,hud)<-stepRuntime (1/120) (control current) runtime current
          assert (show errors) (not (any (isInfixOf "Entity ") errors))
          loop (count+1::Int) nextR nextP hud
  (finalR,final,status)<-loop 0 r started ""
  let tr=transforms (world final) IM.! marble
  assert ("marble course did not finish: "++status++" / "++show tr) ("COURSE COMPLETE" `isInfixOf` status)
  (resetR,reset,errors,_)<-stepRuntime (1/120) [0,0,0,1] finalR final
  stopRuntime resetR
  let V3 x _ z=position (transforms (world reset) IM.! marble)
  assert "restart resets to spawn" (abs x<0.1 && abs (z-8)<0.1 && null errors)
  where clamp=max (-1) . min 1;clampSpeed=max (-4) . min 4

marbleLift :: IO ()
marbleLift = do
  p<-load "marble-run"
  let w=world p
      named n=case [e | (e,name)<-IM.toList (names w),name==n] of e:_->e;[]->error ("Missing level entity: "++n)
      marble=named "Marble";lift=named "Sky lift";bridge=named "Turn right"
      guide=named "Lift / guide 1"
      ride=p {world=w {transforms=IM.adjust (\t->t {position=V3 (-6) 0.52 8}) marble (transforms w)}}
  (r,started,_)<-startRuntime "projects/marble-run" ride
  (finalR,final)<-foldM (\(runtime,current) _->do
    (r',p',err,_)<-stepRuntime (1/120) [0,0,0,0] runtime current
    assert (show err) (not (any (isInfixOf "Entity ") err))
    pure (r',p')) (r,started) [1..540::Int]
  stopRuntime finalR
  let fw=world final
      V3 x y z=position (transforms fw IM.! marble)
      V3 _ ly _=position (transforms fw IM.! lift)
      V3 _ gy _=position (transforms fw IM.! guide)
      V3 _ _ bz=position (transforms fw IM.! bridge)
  assert ("lift did not carry rider: "++show (x,y,z,ly)) (abs (x+6)<0.1 && abs (z-8)<0.1 && y>3.5 && abs (y-ly-1)<0.08)
  assert "rider remains grounded" (grounded (bodies fw IM.! marble))
  assert "lift decorations follow its collider" (abs (gy-ly-0.58)<0.001)
  assert "bridge moved sideways" (abs (bz+5)>0.3)

knockdownGame :: IO ()
knockdownGame = do
  p<-load "knockdown"
  (r,started,_)<-startRuntime "projects/knockdown" p
  (finalR,final)<-foldM (\(runtime,current) _->do
    (r',p',err,_)<-stepRuntime (1/120) [-0.45,-1,0,0] runtime current
    assert (show err) (not (any (isInfixOf "Entity ") err))
    pure (r',p')) (r,started) [1..480::Int]
  stopRuntime finalR
  let targets=[t | (e,t)<-IM.toList (transforms (world final)),maybe False (isInfixOf "Target") (IM.lookup e (names (world final)))]
      tipped t = let V3 x _ z=rotation t in abs x>35 || abs z>35
  assert "no target toppled" (any tipped targets)
