module CircuitBreaker (circuitBreaker,sampleProject,playerComponent,switchComponent,doorComponent,exitComponent) where

import Unholy
import qualified Unholy.ECS as ECS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Control.Monad (unless,forM_)

data PlayerConfig = PlayerConfig Float Float deriving (Eq,Show)
data SwitchConfig = SwitchConfig (Maybe Entity) deriving (Eq,Show)
data DoorConfig = DoorConfig Float Float deriving (Eq,Show)
playerComponent :: Component PlayerConfig
playerComponent=Component "player" (PlayerConfig 5 7) [Field "speed" "Rolling speed" (NumberField 1 15),Field "jump" "Jump speed" (NumberField 1 12)]
  (\(PlayerConfig speed leap)->M.fromList [("speed",FloatValue speed),("jump",FloatValue leap)])
  (\fs->PlayerConfig <$> numberField "speed" fs <*> numberField "jump" fs)
switchComponent :: Component SwitchConfig
switchComponent=Component "switch" (SwitchConfig Nothing) [Field "target" "Door to activate" EntityField]
  (\(SwitchConfig ref)->M.singleton "target" (EntityValue ref)) (\fs->SwitchConfig <$> entityField "target" fs)
doorComponent :: Component DoorConfig
doorComponent=Component "door" (DoorConfig 3 3.5) [Field "speed" "Opening speed" (NumberField 0.2 12),Field "travel" "Vertical travel" (NumberField 2 6)]
  (\(DoorConfig speed travel)->M.fromList [("speed",FloatValue speed),("travel",FloatValue travel)])
  (\fs->DoorConfig <$> numberField "speed" fs <*> numberField "travel" fs)
exitComponent :: Component ()
exitComponent=Component "exit" () [] (const M.empty) (const (Right ()))
definitions :: [SomeComponent]
definitions=[SomeComponent playerComponent,SomeComponent switchComponent,SomeComponent doorComponent,SomeComponent exitComponent]

data DoorState = DoorState DoorConfig Transform Float Bool
data Session = Session
  { playerId :: Entity, exitId :: Entity, controller :: PlayerConfig
  , switches :: ECS.Components SwitchConfig, doors :: IM.IntMap DoorState, activated :: IS.IntSet
  , original :: Project, heldAction :: Bool, heldRestart :: Bool, heldJump :: Bool, pressedAction :: Bool, complete :: Bool }

session :: Project -> Either String Session
session p=do
  ps<-readComponents playerComponent w; ss<-readComponents switchComponent w
  ds<-readComponents doorComponent w; es<-readComponents exitComponent w
  (ball,control)<-one "player" (ECS.query w ps)
  (finish,_)<-one "exit" (ECS.query w es)
  unless (not (null (ECS.query w ss))) (Left "Circuit Breaker needs at least one switch")
  unless (maybe False (\b->mass b>0 && bodyShape b==Sphere) (IM.lookup ball (bodies w))) (Left "Player needs a dynamic sphere body")
  unless (maybe False trigger (IM.lookup finish (bodies w))) (Left "Exit needs a trigger body")
  forM_ (ECS.query w ss) $ \(e,SwitchConfig ref)->do
    targetId<-maybe (Left ("Switch #"++show e++" needs a door target")) Right ref
    unless (maybe False (const True) (ECS.lookup targetId ds)) (Left ("Switch #"++show e++" must target a door component"))
    unless (maybe False trigger (IM.lookup e (bodies w))) (Left ("Switch #"++show e++" needs a trigger body"))
  forM_ (ECS.query w ds) $ \(e,_)->unless (maybe False kinematic (IM.lookup e (bodies w))) (Left ("Door #"++show e++" needs a kinematic body"))
  pure (Session ball finish control ss (IM.fromList [(e,DoorState cfg (transforms w IM.! e) 0 False) | (e,cfg)<-ECS.query w ds]) IS.empty p False False False False False)
  where
    w=world p
    one _ [value]=Right value
    one label _=Left ("Circuit Breaker needs exactly one "++label++" component")

circuitBreaker :: SomeGame
circuitBreaker=game (simpleGame "circuit-breaker" ())
  { components=definitions,validateScene= \p->session p >> Right ()
  , initialize= \_ p->do state<-either (ioError.userError) pure (session p);pure (state,p,[Log "Circuit Breaker / typed authored components online"])
  , beforePhysics=update,afterPhysics=contacts }

update :: System Session
update _ step controls state p
  | restart controls && not (heldRestart state) || fell=do
      fresh<-either (ioError.userError) pure (session (original state))
      pure (fresh {heldRestart=restart controls},original state,[Teleport e | e<-IM.keys (bodies (world (original state)))])
  | otherwise=do
      let dt=deltaTime step; w=world p
          PlayerConfig speed leap=controller state
          magnitude=max 1 (sqrt (moveX controls^ (2::Int)+moveZ controls^ (2::Int)))
          vx=if complete state then 0 else moveX controls/magnitude*speed
          vz=if complete state then 0 else moveZ controls/magnitude*speed
          jumping=jump controls && not (heldJump state)
          control b=let V3 _ vy _=velocity b in b {velocity=V3 vx (if jumping && grounded b then leap else vy) vz}
          nextDoor (DoorState cfg@(DoorConfig opening travel) home amount active)=DoorState cfg home (if active then min travel (amount+opening*dt) else amount) active
          moved=IM.map nextDoor (doors state)
          place current (e,DoorState _ home amount _)=let V3 hx hy hz=position home in setTransform e home {position=V3 hx (hy+amount) hz} current
          animated=foldl place w {bodies=IM.adjust control (playerId state) (bodies w)} (IM.toAscList moved)
          V3 x y z=position (transforms animated IM.! playerId state)
          next=state {doors=moved,heldJump=jump controls,heldRestart=restart controls,heldAction=action controls,pressedAction=action controls && not (heldAction state)}
      pure (next,p {world=animated,camera=(camera p) {eye=V3 (x+7) (y+9) (z+10),target=V3 x (y+0.6) (z-2)}},[])
  where fell=let V3 _ y _=position (transforms (world p) IM.! playerId state) in y<(-5)

contacts :: [CollisionEvent] -> System Session
contacts events _ _ _ state p=do
  let touched=[other | event<-events,collisionTrigger event,collisionKind event/="exit",
        let c=collisionContact event;a=contactA c;b=contactB c,playerId state==a || playerId state==b,let other=if a==playerId state then b else a]
      fired=[(e,targetId) | e<-touched,pressedAction state,IS.notMember e (activated state),Just (SwitchConfig (Just targetId))<-[ECS.lookup e (switches state)]]
      opened=foldr (\(_,e)->IM.adjust (\(DoorState cfg home amount _)->DoorState cfg home amount True) e) (doors state) fired
      active=IS.union (activated state) (IS.fromList (map fst fired))
      total=length (ECS.query (world p) (switches state))
      won=complete state || (IS.size active==total && exitId state `elem` touched)
      color current (e,_)=current {materials=IM.adjust (\m->m {albedo=V3 0.15 1 0.45,emission=1.5}) e (materials current)}
      nextWorld=foldl color (world p) fired
      status=if won then "CIRCUIT COMPLETE / R to restart" else "CIRCUIT BREAKER / "++show (IS.size active)++" of "++show total++" doors\nRoll onto a blue pad and press E. WASD move / Space jump."
      logs=[Log ("Switch #"++show e++" opened door #"++show targetId) | (e,targetId)<-fired]++[Log "Circuit complete" | won && not (complete state)]
  pure (state {doors=opened,activated=active,complete=won},p {world=nextWorld},HUD status:logs)

sampleProject :: Project
sampleProject=(emptyProject "Circuit Breaker") {haskellGame=Just "circuit-breaker",world=scene,
  camera=Camera (V3 11 10 18) (V3 0 0 1) 45,
  environment=defaultEnvironment {skyTop=V3 0.03 0.06 0.14,skyBottom=V3 0.15 0.24 0.32,fogDensity=0.018,sunShadows=True}}
  where
    mesh name shape pos size color body w=let (e,next)=spawnMesh name shape w
      in (e,next {transforms=IM.insert e (Transform pos (V3 0 0 0) size) (transforms next),materials=IM.insert e (defaultMaterial {albedo=color,metallic=0.55,roughness=0.28}) (materials next),bodies=maybe (bodies next) (\b->IM.insert e b (bodies next)) body})
    (_,floorWorld)=mesh "Suspended deck" "cube" (V3 0 (-0.25) 0) (V3 6 0.5 25) (V3 0.18 0.23 0.3) (Just defaultBody) emptyWorld
    (ball,w0)=mesh "Marble" "sphere" (V3 0 0.6 10) (V3 1 1 1) (V3 0.95 0.48 0.12) (Just defaultBody {bodyShape=Sphere,mass=1,friction=0.7}) floorWorld
    w1=setComponent playerComponent ball (PlayerConfig 5 7) w0
    assembly (label,padZ,gateZ) w=let
      (root,a)=spawn (label++" assembly") w
      (gate,b)=mesh (label++" gate") "cube" (V3 0 1.25 gateZ) (V3 5.7 2.5 0.45) (V3 0.09 0.34 0.55) (Just defaultBody {kinematic=True}) a
      (pad,c)=mesh (label++" switch") "cube" (V3 0 0.25 padZ) (V3 2 0.5 2) (V3 0.1 0.55 1) (Just defaultBody {trigger=True}) b
      linked=setComponent switchComponent pad (SwitchConfig (Just gate)) (setComponent doorComponent gate (DoorConfig 3 3.5) c)
      in linked {parents=IM.insert gate (Parent root (transforms linked IM.! gate)) (IM.insert pad (Parent root (transforms linked IM.! pad)) (parents linked))}
    w2=foldr assembly w1 [("North",7,4),("South",0,-3)]
    rails=foldl (\w (x,z)->snd (mesh "Edge light" "cube" (V3 x 0.2 z) (V3 0.15 0.4 1.5) (V3 0.1 0.65 0.85) Nothing w)) w2 [(x,z) | x<-[-2.9,2.9],z<-[-11,-8..10]]
    (finish,w3)=mesh "Delivery portal" "torus" (V3 0 1 (-9)) (V3 4 4 4) (V3 0.2 0.95 0.45) Nothing rails
    (sensor,w4)=spawn "Exit trigger" w3
    scene=setComponent exitComponent sensor () w4 {transforms=IM.insert sensor (Transform (V3 0 1 (-9)) (V3 0 0 0) (V3 4 2 1)) (transforms w4),bodies=IM.insert sensor defaultBody {trigger=True} (bodies w4),lights=IM.singleton finish (Light (V3 0.2 1 0.6) 12)}
