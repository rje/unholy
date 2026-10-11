module PulseCourier (pulseCourier, sampleProject) where

import Unholy
import qualified Unholy.ECS as ECS
import qualified Data.IntMap.Strict as IM
import Data.List (isPrefixOf)
import Text.Printf (printf)

-- These components belong to the game, and are checked by GHC like any other
-- Haskell values. No engine source modification or Lua glue is needed.
data Relay = Relay { relayHome :: V3, relayPhase :: Float }
data Mover = Mover { moverHome :: Transform, moverPhase :: Float }
data Courier = Courier { charge :: Int, dashLeft :: Float, cooldown :: Float }
data Session = Session
  { courier :: Courier, relays :: ECS.Components Relay, movers :: ECS.Components Mover
  , initialScene :: Project, player :: Entity, finish :: Entity
  , time :: Float, won :: Bool, heldJump :: Bool, heldAction :: Bool, heldRestart :: Bool }

pulseCourier :: SomeGame
pulseCourier=game Game
  { identifier="pulse-courier", requiredAssets=map ("assets/audio/"++) ["ambience.wav","jump.wav","gem.wav","finish.wav"]
  , initialize= \_ p->do
      state<-either (ioError.userError) pure (session p)
      pure (state,p,[Log "Pulse Courier: compiled Haskell systems online.",Sound "assets/audio/ambience.wav" 0.12 1 True])
  , beforePhysics=update
  , afterPhysics=contacts }
session :: Project -> Either String Session
session p=do
  ball<-named "Courier"
  exit<-named "Exit sensor"
  case IM.lookup ball (bodies w) of
    Just body | mass body>0 && bodyShape body==Sphere->pure ()
    _->Left "Pulse Courier requires a dynamic sphere body on Courier"
  case IM.lookup exit (bodies w) of
    Just body | trigger body->pure ()
    _->Left "Pulse Courier requires a trigger body on Exit sensor"
  pure (Session (Courier 0 0 0) pickups moving p ball exit 0 False False False False)
  where
    w=world p
    named name=case [e | (e,n)<-IM.toAscList (names w),n==name] of
      [e]->Right e
      _->Left ("Pulse Courier scene requires one entity named "++name)
    pickups=foldr (\(e,n) xs->if "Relay " `isPrefixOf` n then ECS.insert e (Relay (position (transforms w IM.! e)) (fromIntegral e)) xs else xs) ECS.empty (IM.toAscList (names w))
    moving=foldr (\(e,n) xs->if "Shuttle " `isPrefixOf` n then ECS.insert e (Mover (transforms w IM.! e) (fromIntegral e)) xs else xs) ECS.empty (IM.toAscList (names w))

update :: System Session
update _ step input state p
  | restart input && not (heldRestart state) || belowFloor = do
      original<-either (ioError.userError) pure (session (initialScene state))
      let fresh=original {heldRestart=restart input}
      pure (fresh,initialScene state,Log "Courier reset.":[Teleport e | e<-IM.keys (bodies (world (initialScene state)))])
  | otherwise=do
      let dt=deltaTime step;t=time state+dt
          old=courier state
          dash=action input && not (heldAction state) && cooldown old<=0 && not (won state)
          nextCourier=old {dashLeft=if dash then 0.32 else max 0 (dashLeft old-dt),cooldown=if dash then 1.2 else max 0 (cooldown old-dt)}
          speed=if dashLeft nextCourier>0 then 13 else 6
          rawX=moveX input;rawZ=moveZ input;lengthInput=max 1 (sqrt (rawX*rawX+rawZ*rawZ))
          move=if won state then V3 0 0 0 else V3 (speed*rawX/lengthInput) 0 (speed*rawZ/lengthInput)
          body=IM.lookup (player state) (bodies (world p))
          jumping=jump input && not (heldJump state) && maybe False grounded body && not (won state)
          control b=let V3 vx vy vz=velocity b;V3 tx _ tz=move;factor=min 1 (dt*12)
                    in b {velocity=V3 (vx+(tx-vx)*factor) (if jumping then 7 else vy) (vz+(tz-vz)*factor)}
          controlled=(world p) {bodies=IM.adjust control (player state) (bodies (world p))}
          animateRelay w (e,relay)=case IM.lookup e (transforms w) of
            Nothing->w
            Just tr->let V3 x y z=relayHome relay in setTransform e tr {position=V3 x (y+0.14*sin (t*2+relayPhase relay)) z,rotation=V3 0 (t*65) 0} w
          animateMover w (e,mover)=let home=moverHome mover;V3 x y z=position home in
            setTransform e home {position=V3 (x+1.7*sin (t*0.9+moverPhase mover)) y z} w
          animated=foldl animateMover (foldl animateRelay controlled (ECS.query controlled (relays state))) (ECS.query controlled (movers state))
          pos=maybe (V3 0 0 0) position (IM.lookup (player state) (transforms animated))
          V3 px py pz=pos
          cam=Camera (V3 (px*0.3+8) (max 8 (py+8)) (pz+11)) (V3 (px*0.6) 0 (pz-3)) 48
          next=state {courier=nextCourier,time=if won state then time state else t,heldJump=jump input,heldAction=action input,heldRestart=restart input,relays=ECS.prune animated (relays state),movers=ECS.prune animated (movers state)}
          effects=[Sound "assets/audio/jump.wav" 0.35 (if dash then 1.4 else 1) False | jumping || dash]
      pure (next,p {world=animated,camera=cam},effects)
  where belowFloor=case IM.lookup (player state) (transforms (world p)) of Just tr->let V3 _ y _=position tr in y<(-8);_->True

contacts :: [CollisionEvent] -> System Session
contacts events _ _ _ state p=do
  let touched=[other | event<-events,collisionTrigger event,collisionKind event=="enter"
                     ,let contact=collisionContact event; a=contactA contact;b=contactB contact
                     ,player state==a || player state==b,let other=if a==player state then b else a]
      pickups=[e | e<-touched,Just _<-[ECS.lookup e (relays state)]]
      count=charge (courier state)+length pickups
      nextWorld=foldr destroy (world p) pickups
      complete=not (won state) && count>=3 && finish state `elem` touched
      next=state {courier=(courier state) {charge=count},relays=foldr ECS.remove (relays state) pickups,won=won state || complete}
      status=if won next then printf "DELIVERY COMPLETE / %.1fs\nAll relays charged. R plays again." (time next)
             else printf "PULSE COURIER / %d of 3 relays\nWASD roll / Space jump / E dash / R reset\n%s" count (if count>=3 then "Deliver to the green portal!" else "Collect the glowing relay cores.")
      effects=[Sound "assets/audio/gem.wav" 0.5 (1+fromIntegral count*0.08) False | not (null pickups)]
            ++(if complete then [Sound "assets/audio/finish.wav" 0.55 1 False,Log "Pulse Courier delivery complete."] else [])
  pure (next,p {world=nextWorld},HUD status:effects)

-- The project itself is authored with the same public ECS API. Its saved scene
-- remains editable; initialize uses that scene rather than overwriting edits.
sampleProject :: Project
sampleProject=(emptyProject "Pulse Courier")
  { world=scene, haskellGame=Just "pulse-courier"
  , camera=Camera (V3 11 15 21) (V3 0 0 (-1)) 48
  , environment=defaultEnvironment {sunColor=V3 0.65 0.78 1,sunPower=2.3,ambient=0.22,skyTop=V3 0.018 0.027 0.065,skyBottom=V3 0.1 0.16 0.22,fogDensity=0.009,sunShadows=True} }
  where
    metal=Material (V3 0.18 0.23 0.3) 0.72 0.32 0
    cyan=Material (V3 0.08 0.72 0.95) 0.35 0.24 1.5
    orange=Material (V3 1 0.4 0.07) 0.5 0.3 1
    green=Material (V3 0.15 0.95 0.55) 0.4 0.25 1.3
    add name shape pos size rot mat body w=let (e,next)=spawnMesh name shape w
      in (e,next {transforms=IM.insert e (Transform pos rot size) (transforms next),materials=IM.insert e mat (materials next),bodies=maybe (bodies next) (\b->IM.insert e b (bodies next)) body})
    object name shape pos size mat body=add name shape pos size (V3 0 0 0) mat body
    (_,floorWorld)=object "Deck" "cube" (V3 0 (-0.4) 0) (V3 16 0.8 30) metal (Just defaultBody) emptyWorld
    (_,playerWorld)=object "Courier" "sphere" (V3 0 0.6 11) (V3 1 1 1) (Material (V3 0.94 0.7 0.16) 0.78 0.18 0.05) (Just defaultBody {bodyShape=Sphere,mass=1,friction=0.45,angularDamping=0.12}) floorWorld
    rails=foldl (\w x->snd (object "Edge rail" "cube" (V3 x 0.4 0) (V3 0.4 0.8 30) cyan (Just defaultBody) w)) playerWorld [-7.8,7.8]
    strips=foldl (\w x->snd (object "Guide light" "cube" (V3 x 0.018 0) (V3 0.09 0.03 29) cyan Nothing w)) rails [-2.2,2.2]
    tiled=foldl (\w z->snd (object "Deck seam" "cube" (V3 0 0.012 z) (V3 15 0.02 0.055) (Material (V3 0.35 0.43 0.55) 0.3 0.4 0) Nothing w)) strips [-14,-12..14]
    cores=foldl (\w (i,z)->let
      (e,core)=object ("Relay "++show i) "sphere" (V3 0 0.65 z) (V3 0.7 0.7 0.7) orange (Just defaultBody {bodyShape=Sphere,trigger=True}) w
      (_,halo)=add "Relay halo" "torus" (V3 0 0.3 z) (V3 2.3 2.3 2.3) (V3 90 0 0) cyan Nothing core
      h=nextId core
      in halo {parents=IM.insert h (Parent e (relative (transforms halo IM.! e) (transforms halo IM.! h))) (parents halo)}) tiled (zip [1..3::Int] [6,0,-6])
    shuttles=foldl (\w (x,z)->snd (object ("Shuttle "++show z) "cube" (V3 x 0.25 z) (V3 2.3 0.5 2.5) (Material (V3 0.4 0.14 0.65) 0.65 0.24 0.12) (Just defaultBody {kinematic=True}) w)) cores [(4,3),(-4,-3)]
    architecture=foldl tower shuttles [(x,z) | x<-[-6.3,6.3],z<-[-11,-4,4,11]]
    tower w (x,z)=
      let (_,base)=object "Tower footing" "cylinder" (V3 x 0 z) (V3 1.8 0.4 1.8) metal Nothing w
          (_,pillar)=object "Signal tower" "cube" (V3 x 2 z) (V3 0.35 4 0.35) metal Nothing base
      in snd (object "Signal lamp" "sphere" (V3 x 4.2 z) (V3 0.8 0.8 0.8) cyan Nothing pillar)
    (_,portal)=add "Delivery portal" "torus" (V3 0 2 (-11)) (V3 6 6 2) (V3 0 0 0) green Nothing architecture
    (sensor,exitWorld)=spawn "Exit sensor" portal
    scene=exitWorld {transforms=IM.insert sensor (identity {position=V3 0 1 (-11),scale=V3 5 2 1.3}) (transforms exitWorld),bodies=IM.insert sensor defaultBody {trigger=True} (bodies exitWorld)}
