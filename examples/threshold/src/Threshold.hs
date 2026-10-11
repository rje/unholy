module Threshold (threshold,sampleProject) where

import Unholy
import qualified Unholy.ECS as ECS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Control.Monad (unless)

playerComponent :: Component (Float,Float)
playerComponent=Component "traveler" (5,7) [Field "speed" "Walk speed" (NumberField 1 12),Field "jump" "Jump speed" (NumberField 1 12)]
  (\(speed,leap)->M.fromList [("speed",FloatValue speed),("jump",FloatValue leap)])
  (\fields->(,) <$> numberField "speed" fields <*> numberField "jump" fields)
marker :: String -> Component ()
marker key=Component key () [] (const M.empty) (const (Right ()))
platformComponent :: Component (Float,Float)
platformComponent=Component "shuttle" (1.6,4) [Field "travel" "Horizontal travel" (NumberField 0 4),Field "period" "Cycle seconds" (NumberField 1 12)]
  (\(travel,period)->M.fromList [("travel",FloatValue travel),("period",FloatValue period)])
  (\fields->(,) <$> numberField "travel" fields <*> numberField "period" fields)
data Session = Session { traveler :: Entity, finish :: Entity, beacons :: [Entity], shuttles :: [(Entity,Transform,(Float,Float))]
  , speedAndJump :: (Float,Float), home :: Project, clock :: Float, collected :: IS.IntSet, jumpHeld :: Bool, restartHeld :: Bool, won :: Bool }

session :: Project -> Either String Session
session p=do
  ps<-readComponents playerComponent w
  gs<-readComponents (marker "threshold") w
  bs<-readComponents (marker "beacon") w
  lifts<-readComponents platformComponent w
  (player,controls)<-one "traveler" (ECS.query w ps)
  (goal,_)<-one "threshold" (ECS.query w gs)
  unless (maybe False (\b->bodyShape b==Capsule && mass b>0 && lockRotation b) (IM.lookup player (bodies w))) (Left "Traveler needs an upright dynamic capsule")
  unless (not (null (ECS.query w bs)) && all (\e->maybe False trigger (IM.lookup e (bodies w))) (goal:map fst (ECS.query w bs))) (Left "Beacons and threshold need trigger bodies")
  unless (all (\(e,_)->maybe False kinematic (IM.lookup e (bodies w))) (ECS.query w lifts)) (Left "Shuttles need kinematic bodies")
  pure (Session player goal (map fst (ECS.query w bs)) [(e,transforms w IM.! e,cfg) | (e,cfg)<-ECS.query w lifts] controls p 0 IS.empty False False False)
  where w=world p
        one _ [value]=Right value
        one key _=Left ("Threshold needs exactly one "++key)

threshold :: SomeGame
threshold=game (simpleGame "threshold" ())
  { components=[SomeComponent playerComponent,SomeComponent (marker "beacon"),SomeComponent (marker "threshold"),SomeComponent platformComponent]
  , requiredAssets=["assets/audio/jump.wav","assets/audio/pickup.wav"]
  , validateScene= \p->session p >> Right ()
  , initialize= \_ p->do state<-either (ioError.userError) pure (session p);pure (state,p,[Log "Threshold / capsule traversal and mesh collision online"])
  , beforePhysics=move,afterPhysics=touch }

move :: System Session
move _ step controls state p
  | restart controls && not (restartHeld state) || fell=do
      fresh<-either (ioError.userError) pure (session (home state))
      pure (fresh {restartHeld=restart controls},home state,[Teleport e | e<-IM.keys (bodies (world (home state)))])
  | otherwise=do
      let w=world p;time=clock state+deltaTime step;(speed,leap)=speedAndJump state
          magnitude=max 1 (sqrt (moveX controls^(2::Int)+moveZ controls^(2::Int)))
          jumping=jump controls && not (jumpHeld state) && maybe False grounded (IM.lookup (traveler state) (bodies w))
          velocityFor b=let V3 _ vy _=velocity b;moving=if won state then 0 else speed/magnitude
                       in b {velocity=V3 (moveX controls*moving) (if jumping then leap else vy) (moveZ controls*moving)}
          shuttle current (e,pose,(travel,period))=let V3 x y z=position pose in setTransform e pose {position=V3 (x+travel*sin (time*2*pi/period)) y z} current
          clip=if won state then "idle" else if jumping || not (maybe False grounded (IM.lookup (traveler state) (bodies w))) then "jump" else if abs (moveX controls)+abs (moveZ controls)>0.01 then "walk" else "idle"
          animate a=a {animationClip=clip,animationTime=if animationClip a==clip then animationTime a else 0,animationRate=if clip=="walk" then 1.7 else 1}
          animated=foldl shuttle w {animators=IM.adjust animate (traveler state) (animators w),bodies=IM.adjust velocityFor (traveler state) (bodies w)} (shuttles state)
          V3 x y z=position (transforms animated IM.! traveler state)
          next=state {clock=time,jumpHeld=jump controls,restartHeld=restart controls}
      pure (next,p {world=animated,camera=(camera p) {eye=V3 (x+7) (y+7) (z+10),target=V3 x y (z-3)}},[Sound "assets/audio/jump.wav" 0.25 0.8 False | jumping])
  where fell=let V3 _ y _=position (transforms (world p) IM.! traveler state) in y<(-6)

touch :: [CollisionEvent] -> System Session
touch events _ _ _ state p=do
  let touched=[if a==traveler state then b else a | event<-events,collisionTrigger event,collisionKind event/="exit",let c=collisionContact event;a=contactA c;b=contactB c,a==traveler state || b==traveler state]
      new=[e | e<-beacons state,e `elem` touched,IS.notMember e (collected state)]
      found=IS.union (collected state) (IS.fromList new)
      complete=won state || (IS.size found==length (beacons state) && finish state `elem` touched)
      activate w e=w {bodies=IM.delete e (bodies w),materials=IM.adjust (\m->m {albedo=V3 0.1 0.9 1,emission=2}) e (materials w)}
      message=if complete then "THRESHOLD REACHED / R or Start to restart" else "THRESHOLD / "++show (IS.size found)++" of "++show (length (beacons state))++" beacons\nWASD or left stick / Space or A jump / R or Start reset"
  pure (state {collected=found,won=complete},p {world=foldl activate (world p) new},HUD message:[Sound "assets/audio/pickup.wav" 0.4 0.8 False | not (null new)]++[Log "Threshold reached" | complete && not (won state)])

sampleProject :: Project
sampleProject=(emptyProject "Threshold") {haskellGame=Just "threshold",world=scene,camera=Camera (V3 9 9 21) (V3 0 1 3) 45,
  environment=defaultEnvironment {environmentMap="assets/studio.hdr",environmentIntensity=0.6,environmentRotation=25,sunShadows=True,sunPower=2,skyTop=V3 0.025 0.04 0.065,skyBottom=V3 0.06 0.11 0.13,fogDensity=0.008}}
  where
    mesh name shape pos size color body w=let (e,next)=spawnMesh name shape w in (e,next {transforms=IM.insert e (Transform pos (V3 0 0 0) size) (transforms next),materials=IM.insert e (Material color 0.35 0.38 0) (materials next),bodies=maybe (bodies next) (\b->IM.insert e b (bodies next)) body})
    terrain=generateTerrain 65 65 0.625 (\x z->let ridge=max 0 (min 1 ((abs x-3)/6)) in ridge*(1.7+1.2*sin (x*0.38)*cos (z*0.24))+if z>7 && abs x<3 then 0.08*sin ((z-7)*pi/7) else 0)
    (ground,terrainWorld)=spawnTerrain "Mossy finite terrain" terrain emptyWorld
    base=terrainWorld {meshes=IM.adjust (\m->m {texture="assets/ground.png"}) ground (meshes terrainWorld),materials=IM.insert ground (Material (V3 0.3 0.42 0.26) 0 0.88 0) (materials terrainWorld)}
    (player,a)=mesh "Traveler" "assets/traveler.gltf" (V3 0 1.05 11) (V3 1 1 1) (V3 0.95 0.55 0.16) (Just defaultBody {bodyShape=Capsule,mass=1,lockRotation=True,friction=0.25}) base
    (_,b)=mesh "Sloping causeway / triangle mesh" "assets/ramp.gltf" (V3 0 0 0) (V3 1 1 1) (V3 0.2 0.29 0.3) (Just defaultBody {bodyShape=TriangleMesh}) a
    (_,c)=mesh "Upper terrace" "cube" (V3 0 1 (-7)) (V3 6 2 14) (V3 0.19 0.28 0.29) (Just defaultBody) b
    (_,d)=mesh "Jump threshold" "cube" (V3 0 2.45 (-5)) (V3 5.9 0.9 0.5) (V3 0.55 0.31 0.1) (Just defaultBody) c
    (_,e)=mesh "Pushable convex block" "cube" (V3 0 0.4 7) (V3 0.8 0.8 0.8) (V3 0.4 0.25 0.13) (Just defaultBody {bodyShape=Convex,mass=0.7,friction=0.3}) d
    (lift,f)=mesh "Side shuttle" "cube" (V3 5 2 (-5)) (V3 3 0.4 4) (V3 0.14 0.38 0.44) (Just defaultBody {kinematic=True}) e
    marked=setComponent playerComponent player (5,7) (setComponent platformComponent lift (1.6,4) f {animators=IM.singleton player defaultAnimator {animationClip="idle"}})
    withBeacons=foldl (\w (index,pos)->let (id,next)=mesh ("Beacon "++show index) "torus" pos (V3 1.3 1.3 1.3) (V3 0.95 0.6 0.12) (Just defaultBody {trigger=True}) w in setComponent (marker "beacon") id () next) marked [(1::Int,V3 0 1 9),(2,V3 0 2 3),(3,V3 0 3 (-9))]
    (_,arch)=mesh "Arrival arch" "torus" (V3 0 4 (-12)) (V3 7 7 7) (V3 0.15 0.7 0.8) Nothing withBeacons
    (goal,end)=spawn "Threshold trigger" arch
    portal=setComponent (marker "threshold") goal () end {transforms=IM.insert goal (identity {position=V3 0 3 (-12),scale=V3 4 2 1}) (transforms end),bodies=IM.insert goal defaultBody {trigger=True} (bodies end)}
    scene=foldl (\w (x,z)->snd (mesh "Ruined column" "cylinder" (V3 x 0 z) (V3 0.8 5 0.8) (V3 0.25 0.29 0.3) Nothing w)) portal [(x,z) | x<-[-5,5],z<-[-12,-6,0,6,12]]
