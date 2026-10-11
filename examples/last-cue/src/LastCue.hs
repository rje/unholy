{-# LANGUAGE OverloadedStrings #-}

module LastCue (lastCue, sampleProject, secondProject, roleComponent) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import System.FilePath ((</>))
import Unholy
import qualified Unholy.Animation as Animation
import Unholy.Checkpoint
import qualified Unholy.ECS as ECS

roleComponent :: Component String
roleComponent =
  Component
    "stage-role"
    "prop"
    [Field "key" "Stage role" TextField]
    (\key -> M.singleton "key" (TextValue key))
    (textField "key")

data Phase = Title | Performing | Paused | Caught | CurtainCall deriving (Eq, Show)

data Checkpoint = Checkpoint {savedAct :: String, savedMark :: String, savedKeys :: [String], savedRaised :: Bool} deriving (Eq, Show)

instance ToJSON Checkpoint where
  toJSON c = object ["version" .= (1 :: Int), "act" .= savedAct c, "mark" .= savedMark c, "keys" .= savedKeys c, "raised" .= savedRaised c]

instance FromJSON Checkpoint where
  parseJSON = withObject "Last Cue checkpoint" $ \o -> do
    version <- o .: "version"
    unless (version == (1 :: Int)) (fail "Unsupported checkpoint version")
    c <- Checkpoint <$> o .: "act" <*> o .: "mark" <*> o .: "keys" <*> o .: "raised"
    unless
      ( savedAct c `elem` ["dock", "flies"]
          && savedMark c `elem` ["entrance", "winch"]
          && all (`elem` ["cue-a", "cue-b"]) (savedKeys c)
          && length (savedKeys c) <= 2
          && (not (savedRaised c) || all (`elem` savedKeys c) ["cue-a", "cue-b"])
      )
      (fail "Invalid checkpoint progress")
    pure c

data Layout = Layout
  { actor :: Entity,
    visual :: Entity,
    control :: Entity,
    gate :: Entity,
    exit :: Entity,
    lift :: Entity,
    enemy :: Maybe Entity,
    cues :: [(Entity, String)],
    act :: String
  }

data State = State
  { layout :: Layout,
    home :: Project,
    phase :: Phase,
    checkpoint :: Maybe Checkpoint,
    found :: Set.Set String,
    raised :: Bool,
    yaw :: Float,
    pitch :: Float,
    timer :: Float,
    jumpQueued :: Bool,
    interactQueued :: Bool,
    stunnedUntil :: Float,
    whistleReady :: Float,
    needsSave :: Bool,
    message :: String,
    footstepAt :: Float
  }

readLayout :: Project -> Either String Layout
readLayout p = do
  rows <- readComponents roleComponent (world p)
  let entries = ECS.query (world p) rows
      one key = case [e | (e, k) <- entries, k == key] of [e] -> Right e; _ -> Left ("Last Cue needs exactly one " ++ key)
      optional key = case [e | (e, k) <- entries, k == key] of [] -> Right Nothing; [e] -> Right (Just e); _ -> Left ("Duplicate " ++ key)
  player <- one "player"
  model <- one "visual"
  winch <- one "winch"
  door <- one "gate"
  end <- one "exit"
  platform <- one "lift"
  sentinel <- optional "enemy"
  let pickups = [(e, key) | (e, key) <- entries, key `elem` ["cue-a", "cue-b"]]
      level = if any ((== "act-one") . snd) entries then "dock" else "flies"
  unless (map snd pickups == ["cue-a", "cue-b"]) (Left "Two distinct brass cue keys are required")
  unless (maybe False (\b -> bodyShape b == Capsule && mass b > 0 && lockRotation b) (IM.lookup player (bodies (world p)))) (Left "Stagehand needs an upright dynamic capsule")
  unless (all (\e -> maybe False trigger (IM.lookup e (bodies (world p)))) (end : map fst pickups)) (Left "Cue keys and exit need triggers")
  unless (all (\e -> maybe False kinematic (IM.lookup e (bodies (world p)))) [door, platform]) (Left "Scenery and lift need kinematic bodies")
  unless (IM.member model (animators (world p))) (Left "Stagehand visual needs an Animator")
  pure (Layout player model winch door end platform sentinel pickups level)

lastCue :: SomeGame
lastCue =
  gameWithInterface
    (simpleGame "last-cue" ())
      { components = [SomeComponent roleComponent],
        requiredAssets = ["assets/audio/step.wav", "assets/audio/key.wav", "assets/audio/winch.wav", "assets/audio/whistle.wav", "assets/audio/curtain.wav", "assets/audio/room.wav"],
        validateScene = \p -> readLayout p >> Right (),
        initialize = \ctx p -> do
          l <- either (ioError . userError) pure (readLayout p)
          loaded <- readCheckpoint (saveRoot ctx </> "checkpoint.json")
          saved <- case levelData ctx of
            Nothing -> pure (either (const Nothing) id loaded)
            Just payload -> case fromJSON payload of Error err -> ioError (userError err); Success value -> pure (Just value)
          let warningText = either ("Checkpoint unavailable: " ++) (const "") loaded
              start = State l p (if enteringLevel ctx then Performing else Title) saved Set.empty False 0 0.42 0 False False 0 0 (enteringLevel ctx) warningText 0
              (state, next) = if enteringLevel ctx then restoreCheckpoint start p saved else (start, p)
              warning = either (\err -> [Log ("Checkpoint unavailable: " ++ err)]) (const []) loaded
          pure (state, next, screen state ++ warning ++ [Log ("Last Cue / " ++ act l ++ " / Haskell stage systems ready")]),
        beforePhysics = perform,
        afterPhysics = contacts
      }
    frame
    [("dock", "dock.hj.json"), ("flies", "flies.hj.json")]

screen :: State -> [Effect]
screen s = [PauseSimulation (phase s /= Performing), Present (Presentation panel prompt)]
  where
    panel = case phase s of
      Title ->
        Just
          ( Panel
              "LAST CUE"
              ("A stagehand. An empty house. One last show.\nReach the fly tower and bring down the curtain.\n" ++ message s)
              ([("new", "New performance")] ++ [("continue", "Continue checkpoint") | checkpoint s /= Nothing] ++ [("quit", "Exit")])
          )
      Paused -> Just (Panel "INTERMISSION" ("Progress is saved at named stage marks.\n" ++ message s) [("resume", "Resume"), ("save", "Save checkpoint"), ("retry", "Restart checkpoint"), ("quit", "Exit")])
      Caught -> Just (Panel "MISSED YOUR CUE" "The mechanical performer caught you.\nYour saved stage mark is safe." [("retry", "Retry checkpoint"), ("new", "New performance"), ("quit", "Exit")])
      CurtainCall -> Just (Panel "CURTAIN CALL" "The house is dark. The machinery is quiet.\nYou made it through both acts." [("new", "Encore"), ("quit", "Exit")])
      Performing -> Nothing
    prompt =
      if phase s /= Performing
        then ""
        else
          "WASD / left stick move   Space / A jump   RMB / right stick camera\nE / X operate winch or whistle   Esc / Start pause"
            ++ if take 12 (message s) == "Save failed:" then "\nProgress is in memory; saving is unavailable." else ""

frame :: GameContext -> Float -> ActionFrame -> State -> Project -> IO (State, Project, [Effect])
frame ctx _dt input s p = do
  let clicked key = key `elem` activatedButtons input
      (lookX, lookY) = lookDelta input
      looking = s {yaw = yaw s + lookX, pitch = max 0.18 (min 1.15 (pitch s + lookY))}
      pending = looking {jumpQueued = jumpQueued s || actionPressed "jump" input, interactQueued = interactQueued s || actionPressed "interact" input}
      atMark = Checkpoint (act (layout s)) (if raised s then "winch" else "entrance") (Set.toList (found s)) (raised s)
      save current = do
        result <- writeCheckpoint (saveRoot ctx </> "checkpoint.json") atMark
        let updated = current {checkpoint = Just atMark, needsSave = False, message = either ("Save failed: " ++) (const "Checkpoint saved") result}
        pure (updated, p, [Log (message updated)] ++ screen updated)
  if clicked "quit"
    then pure (s, p, [Request ExitGame])
    else
      if clicked "new"
        then do
          let fresh = Checkpoint "dock" "entrance" [] False
          result <- writeCheckpoint (saveRoot ctx </> "checkpoint.json") fresh
          let notice = either ("Save failed: " ++) (const "") result
          if act (layout s) /= "dock"
            then pure (s, p, [Request (LoadLevelWith "dock" (toJSON fresh))])
            else do
              let reset = s {phase = Performing, checkpoint = Just fresh, found = Set.empty, raised = False, timer = 0, yaw = 0, pitch = 0.42, stunnedUntil = 0, whistleReady = 0, footstepAt = 0, jumpQueued = False, interactQueued = False, message = notice, needsSave = False}
              pure (reset, home s, screen reset ++ [Teleport e | e <- IM.keys (bodies (world (home s)))] ++ [Sound "assets/audio/room.wav" 0.12 1 True])
        else
          if clicked "continue"
            then case checkpoint s of
              Just saved | savedAct saved /= act (layout s) -> pure (s, p, [Request (LoadLevelWith (savedAct saved) (toJSON saved))])
              saved ->
                let (restored, next) = restoreCheckpoint s {phase = Performing} p saved
                 in pure (restored, next, screen restored ++ [Teleport e | e <- IM.keys (bodies (world next))] ++ [Sound "assets/audio/room.wav" 0.12 1 True])
            else
              if clicked "retry"
                then pure (s, p, [Request (LoadLevelWith (act (layout s)) (toJSON (maybe (Checkpoint (act (layout s)) "entrance" [] False) id (checkpoint s))))])
                else
                  if clicked "save"
                    then save s
                    else
                      if clicked "resume" || phase s == Paused && actionPressed "pause" input
                        then let next = s {phase = Performing, jumpQueued = False, interactQueued = False} in pure (next, p, screen next)
                        else
                          if phase s == Performing && actionPressed "pause" input
                            then let next = s {phase = Paused, jumpQueued = False, interactQueued = False} in pure (next, p, screen next)
                            else
                              if phase s == Performing && needsSave s
                                then save pending
                                else pure (if phase s == Performing then pending else s, p, [])

restoreCheckpoint :: State -> Project -> Maybe Checkpoint -> (State, Project)
restoreCheckpoint s _ saved = (updated, (home s) {world = placed})
  where
    l = layout s
    progress = case saved of Just c | savedAct c == act l -> c; _ -> Checkpoint (act l) "entrance" [] False
    updated = s {found = Set.fromList (savedKeys progress), raised = savedRaised progress, jumpQueued = False, interactQueued = False}
    withoutKeys = foldl (\w (e, key) -> if key `elem` savedKeys progress then destroy e w else w) (world (home s)) (cues l)
    opened = if savedRaised progress then raiseGate l withoutKeys else withoutKeys
    marker = if savedMark progress == "winch" then "checkpoint" else "entrance"
    marked = [position t | (e, t) <- IM.toList (transforms opened), Just roles <- [IM.lookup e (authored opened)], Just fields <- [M.lookup "stage-role" roles], M.lookup "key" fields == Just (TextValue marker)]
    placed = case marked of point : _ -> setTransform (actor l) (transforms opened IM.! actor l) {position = point} opened; _ -> opened

raiseGate :: Layout -> World -> World
raiseGate l w = setTransform (gate l) (transforms w IM.! gate l) {position = let V3 x _ z = position (transforms w IM.! gate l) in V3 x 9 z} w

perform :: System State
perform ctx step controls s p
  | phase s /= Performing = pure (s, p, [])
  | otherwise = do
      let l = layout s
          w = world p
          time = timer s + deltaTime step
          operator = position (transforms w IM.! actor l)
          closeTo e = distance operator (position (transforms w IM.! e)) < 2.2
          operating = interactQueued s && closeTo (control l) && Set.size (found s) == 2 && not (raised s)
          whistling = interactQueued s && not operating && time >= whistleReady s && maybe False closeTo (enemy l)
          updated =
            s
              { timer = time,
                jumpQueued = False,
                interactQueued = False,
                raised = raised s || operating,
                needsSave = needsSave s || operating,
                stunnedUntil = if whistling then time + 3.5 else stunnedUntil s,
                whistleReady = if whistling then time + 5 else whistleReady s
              }
          pose = transforms (world (home s)) IM.! lift l
          V3 px py pz = position pose
          height = if raised updated then 1.5 + 1.5 * sin (time * 0.8 - pi / 2) else 0
          moving = setTransform (lift l) pose {position = V3 px (py + height) pz} (if raised updated then raiseGate l w else w)
      (driven, jumping, facing) <- driveCharacter (gamePhysics ctx) defaultCharacter (yaw s) (jumpQueued s) controls (actor l) moving
      let V3 vx _ vz = velocity (bodies driven IM.! actor l)
          walking = abs vx + abs vz > 0.05
          clip = if jumping || not (grounded (bodies driven IM.! actor l)) then "jump" else if walking then "walk" else "idle"
          visualPose = transforms driven IM.! visual l
          animated =
            setTransform
              (visual l)
              visualPose {rotation = if walking then V3 0 facing 0 else rotation visualPose}
              driven
                { animators = IM.adjust (Animation.crossfade 0.18 clip) (visual l) (animators driven)
                }
          stepping = walking && grounded (bodies driven IM.! actor l) && time >= footstepAt s
      patrolled <- patrol ctx updated animated
      camera' <- followCamera (gamePhysics ctx) patrolled (actor l) (yaw s) (pitch s)
      let hint =
            if closeTo (control l)
              then if raised updated then "Winch engaged. Ride the scenery lift." else if Set.size (found s) < 2 then "Find both brass cue keys to unlock the winch." else "E / X: engage the scenery winch."
              else if maybe False closeTo (enemy l) then "E / X: whistle to stun the performer. Keep moving!" else if raised updated then "Reach the upper exit." else "Collect the two brass cue keys."
          label = "LAST CUE / " ++ (if act l == "dock" then "ACT I - SCENE DOCK" else "ACT II - FLY TOWER") ++ "\nCue keys " ++ show (Set.size (found s)) ++ " / 2   |   " ++ hint
          effects =
            [HUD label]
              ++ [Sound "assets/audio/key.wav" 0.3 0.8 False | jumping]
              ++ concat [[Sound "assets/audio/winch.wav" 0.55 1 False, Log "Scenery winch engaged"] | operating]
      pure
        ( updated {footstepAt = if stepping then time + 0.38 else footstepAt s},
          p {world = patrolled, camera = camera'},
          effects
            ++ concat [[Sound "assets/audio/whistle.wav" 0.5 1 False, Log "Whistle stunned the performer"] | whistling]
            ++ [Sound "assets/audio/step.wav" 0.20 1 False | stepping]
        )

patrol :: GameContext -> State -> World -> IO World
patrol ctx s w = case enemy (layout s) of
  Nothing -> pure w
  Just e | timer s < stunnedUntil s -> pure w {animators = IM.adjust (Animation.crossfade 0.2 "idle") e (animators w)}
  Just e -> do
    let pose = transforms w IM.! e; from = position pose; to = position (transforms w IM.! actor (layout s))
    obstruction <- raycastSolid (gamePhysics ctx) w from to (Just e)
    let visible = distance from to < 6 && maybe True ((== actor (layout s)) . hitEntity) obstruction
        target' = if visible then to else V3 (4 * sin (timer s * 0.6)) 1.05 (-2)
        V3 x y z = from
        V3 tx _ tz = target'
        dx = tx - x
        dz = tz - z
        length' = max 0.01 (sqrt (dx * dx + dz * dz))
        stride = min length' (delta * if visible then 2.5 else 1.4)
        next = pose {position = V3 (x + dx / length' * stride) y (z + dz / length' * stride), rotation = V3 0 (atan2 (-dx) (-dz) * 180 / pi) 0}
        V3 nx ny nz = position next
        ray side =
          raycastSolid
            (gamePhysics ctx)
            w
            (V3 (x + side * dz / length') (y - 0.55) (z - side * dx / length'))
            (V3 (nx + side * dz / length' + dx / length' * 0.55) (ny - 0.55) (nz - side * dx / length' + dz / length' * 0.55))
            (Just e)
    hits <- mapM ray [-0.4, 0, 0.4]
    let blocked = any (maybe False ((/= actor (layout s)) . hitEntity)) hits
    pure (setTransform e (if blocked then pose else next) w {animators = IM.adjust (Animation.crossfade 0.2 (if blocked then "idle" else "walk")) e (animators w)})
  where
    delta = 1 / 120

contacts :: [CollisionEvent] -> System State
contacts events _ _ _ s p
  | phase s /= Performing = pure (s, p, [])
  | otherwise = do
      let l = layout s
          touched = [if a == actor l then b else a | event <- events, collisionKind event /= "exit", let c = collisionContact event; a = contactA c; b = contactB c, a == actor l || b == actor l]
          collected = [(e, key) | (e, key) <- cues l, e `elem` touched, Set.notMember key (found s)]
          keys = Set.union (found s) (Set.fromList (map snd collected))
          V3 _ height _ = position (transforms (world p) IM.! actor l)
          captured = height < (-4) || maybe False (\e -> distance (position (transforms (world p) IM.! e)) (position (transforms (world p) IM.! actor l)) < 1.0 && timer s >= stunnedUntil s) (enemy l)
          arrived = raised s && exit l `elem` touched
          next = s {found = keys, needsSave = needsSave s || not (null collected), phase = if captured then Caught else if arrived && act l == "flies" then CurtainCall else phase s}
          commands = [Request (LoadLevelWith "flies" (toJSON (Checkpoint "flies" "entrance" [] False))) | arrived && act l == "dock"]
          effects =
            [Sound "assets/audio/key.wav" 0.5 1 False | not (null collected)]
              ++ [Log ("Collected " ++ key) | (_, key) <- collected]
              ++ commands
              ++ concat [[Sound "assets/audio/curtain.wav" 0.5 1 False, Log "Curtain call - both acts complete"] | arrived && act l == "flies"]
      pure (next, p {world = foldl (flip destroy) (world p) (map fst collected), camera = if phase next == CurtainCall then Camera (V3 6 7 12) (V3 0 2 (-3)) 48 else camera p}, effects ++ if phase next /= phase s then screen next else [])

distance :: V3 -> V3 -> Float
distance (V3 x y z) (V3 a b c) = sqrt ((x - a) ^ (2 :: Int) + (y - b) ^ (2 :: Int) + (z - c) ^ (2 :: Int))

sampleProject, secondProject :: Project
sampleProject = scene "dock"
secondProject = scene "flies"

scene :: String -> Project
scene level =
  (emptyProject (if level == "dock" then "Last Cue" else "Last Cue / Fly Tower"))
    { haskellGame = Just "last-cue",
      world = decorated,
      camera = Camera (V3 6 7 12) (V3 0 2 (-3)) 48,
      environment =
        defaultEnvironment
          { sunShadows = True,
            sunDirection = V3 (-0.3) (-1) (-0.4),
            sunColor = V3 1 0.86 0.70,
            sunPower = 1.9,
            ambient = 0.22,
            skyTop = V3 0.012 0.017 0.025,
            skyBottom = V3 0.03 0.025 0.035,
            fogDensity = 0.008,
            environmentMap = "assets/studio.hdr",
            environmentIntensity = 0.32
          }
    }
  where
    role key e = setComponent roleComponent e key
    add key shape pos size color body w =
      let (e, next) = spawnMesh key shape w
       in (e, role key e next {transforms = IM.insert e (Transform pos (V3 0 0 0) size) (transforms next), meshes = IM.adjust (\m -> m {useImportedMaterials = take 7 shape == "assets/"}) e (meshes next), materials = IM.insert e (Material color 0.25 0.55 0) (materials next), bodies = maybe (bodies next) (\collider -> IM.insert e collider (bodies next)) body})
    cream = V3 0.72 0.58 0.38
    brass = V3 0.78 0.52 0.18
    blue = V3 0.15 0.23 0.30
    (_, floorWorld) = add "Stage timber deck" "assets/deck.gltf" (V3 0 (-0.4) 0) (V3 18 0.8 26) (V3 0.8 0.66 0.5) (Just defaultBody) emptyWorld
    (_, back) = add "Rear brick wall" "assets/wall.gltf" (V3 0 4 (-14)) (V3 19 8 0.6) blue (Just defaultBody) floorWorld
    (_, left) = add "Left wing wall" "assets/wing-wall.gltf" (V3 (-9) 4 0) (V3 0.6 8 28) blue (Just defaultBody) back
    (_, right) = add "Right wing wall" "assets/wing-wall.gltf" (V3 9 4 0) (V3 0.6 8 28) blue (Just defaultBody) left
    (player, playerWorld) = spawn "Stagehand capsule" right
    actorWorld = role "player" player playerWorld {transforms = IM.insert player identity {position = V3 0 1.05 9} (transforms playerWorld), bodies = IM.insert player defaultBody {bodyShape = Capsule, mass = 1, friction = 0.1, lockRotation = True} (bodies playerWorld)}
    (model, modelWorld) = add "visual" "assets/stagehand.gltf" (V3 0 1.05 9) (V3 1 1 1) cream Nothing actorWorld
    skinned = modelWorld {meshes = IM.adjust (\m -> m {useImportedMaterials = True}) model (meshes modelWorld), animators = IM.singleton model defaultAnimator {animationClip = "idle"}, parents = IM.singleton model (Parent player identity)}
    (a, first) = add "cue-a" "assets/key.gltf" (V3 (-4) 1 5) (V3 1 1 1) brass (Just defaultBody {trigger = True}) skinned
    (b, second) = add "cue-b" "assets/key.gltf" (V3 4 1 1) (V3 1 1 1) brass (Just defaultBody {trigger = True}) first
    glowing = second {materials = foldr (IM.adjust (\m -> m {emission = 0.3})) (materials second) [a, b]}
    (_, controlled) = add "winch" "assets/winch.gltf" (V3 0 0 (-4)) (V3 1 1 1) brass (Just defaultBody) glowing
    (_, closed) = add "gate" "assets/gate.gltf" (V3 0 1.5 (-6)) (V3 5 3 0.4) (V3 0.65 0.30 0.25) (Just defaultBody {kinematic = True}) controlled
    (_, platform) = add "lift" "assets/lift.gltf" (V3 0 0.12 (-8)) (V3 4.4 0.24 3) cream (Just defaultBody {kinematic = True}) closed
    (_, balcony) = add "Upper fly gallery" "assets/gallery.gltf" (V3 0 2.8 (-11.8)) (V3 16 0.4 4.6) cream (Just defaultBody) platform
    (_, end) = add "exit" "assets/exit.gltf" (V3 0 4 (-12.3)) (V3 3 2 1) (V3 0.18 0.38 0.34) (Just defaultBody {trigger = True}) balcony
    mark key pos w = let (e, next) = spawn key w in role key e next {transforms = IM.insert e identity {position = pos} (transforms next)}
    marked = mark (if level == "dock" then "act-one" else "act-two") (V3 0 0 0) (mark "entrance" (V3 0 1.05 9) (mark "checkpoint" (V3 0 1.05 (-3)) end))
    withEnemy =
      if level == "dock"
        then marked
        else
          let (e, next) = add "enemy" "assets/performer.gltf" (V3 5 1.05 (-2)) (V3 1 1 1) (V3 0.85 0.15 0.12) (Just defaultBody {bodyShape = Capsule, kinematic = True}) marked
           in next {animators = IM.insert e defaultAnimator {animationClip = "walk"} (animators next)}
    scenery = foldl (\w (x, z) -> snd (add "Scenery flat" "assets/flat.gltf" (V3 x 0 z) (V3 1 1 1) cream (Just defaultBody {bodyShape = TriangleMesh}) w)) withEnemy [(-6, 0), (6, -5), (-6, -9)]
    props = foldl (\w (x, z) -> snd (add "Rigging pulley" "assets/pulley.gltf" (V3 x 5 z) (V3 1 1 1) brass Nothing w)) scenery [(x, z) | x <- [-6, -2, 2, 6], z <- [-12, -5, 3]]
    ropes = foldl (\w (x, z) -> snd (add "Suspension rope" "cylinder" (V3 x 0 z) (V3 0.055 8 0.055) cream Nothing w)) props [(x, z) | x <- [-7, -2.3, 2.3, 7], z <- [-12, -8, 0]]
    curtains = foldl (\w (x, z) -> snd (add "Crimson house curtain" "assets/curtain.gltf" (V3 x 0 z) (V3 1 1 1) (V3 0.85 0.4 0.4) Nothing w)) ropes [(-7.8, -13.5), (7.8, -13.5), (-8.2, 11), (8.2, 11)]
    framed = snd (add "Proscenium arch" "assets/arch.gltf" (V3 0 0 (-13)) (V3 1 1 1) brass Nothing curtains)
    overhead = foldl (\w z -> snd (add "Overhead grid timber" "cube" (V3 0 8 z) (V3 18 0.24 0.22) cream Nothing w)) framed [-13, -8, -3, 2, 7, 12]
    dressed =
      if level == "dock"
        then overhead
        else
          let weights = foldl (\w (x, z) -> snd (add "Fly counterweight stack" "assets/counterweight.gltf" (V3 x 0 z) (V3 1 1 1) (V3 0.28 0.32 0.36) (Just defaultBody) w)) overhead [(-7, 5), (7, 5), (-7, -6), (7, -10)]
              bars = foldl (\w (z, h) -> snd (add "Suspended scenery fly bar" "cube" (V3 0 h z) (V3 15 0.13 0.16) (V3 0.3 0.36 0.42) Nothing w)) weights [(6, 4.5), (0, 6), (-6, 7)]
           in foldl (\w x -> snd (add "Gallery side railing" "cube" (V3 x 3.7 (-11.8)) (V3 0.12 1.4 4.5) brass (Just defaultBody) w)) bars [-7.6, 7.6]
    decorated =
      foldl
        ( \w (x, z) ->
            let (e, next) = spawnMesh "Stage worklight" "sphere" w
                color = if level == "flies" then V3 0.46 0.66 1 else V3 1 0.75 0.48
                lamp = next {transforms = IM.insert e identity {position = V3 x 4.8 z, scale = V3 0.28 0.10 0.28} (transforms next), materials = IM.insert e (Material color 0 0.3 4) (materials next), lights = IM.insert e (Light color 18) (lights next)}
             in snd (add "Iron worklight housing" "assets/worklight.gltf" (V3 x 5 z) (V3 1 1 1) cream Nothing lamp)
        )
        dressed
        [(-5, 6), (5, -1), (-5, -9), (5, -12)]
