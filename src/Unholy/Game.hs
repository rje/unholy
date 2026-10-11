{-# LANGUAGE ExistentialQuantification #-}

-- A game keeps its own statically typed state. Only the registration boundary
-- hides that type; there is no Dynamic component store or scripting ABI.
module Unholy.Game
  ( Game (..),
    SomeGame,
    game,
    gameWithInterface,
    FrameSystem,
    gameIdentifier,
    GameContext (..),
    Step (..),
    System,
    Effect (..),
    checkGameAssets,
    gameAssetReferences,
    gameComponents,
    validateGame,
    simpleGame,
    acquireGameResource,
    Panel (..),
    Presentation (..),
    emptyPresentation,
    Command (..),
    frameGame,
    interactiveGame,
    gameLevels,
    RunningGame,
    startGame,
    tickGame,
    applyEffects,
    findGame,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM, forM_, when)
import Data.Aeson (Value)
import Data.List (isSuffixOf, nub)
import System.FilePath (takeDirectory, (</>))
import qualified Unholy.Audio as Audio
import Unholy.Authoring
import Unholy.Exception (trySync)
import Unholy.Input
import Unholy.Physics
import Unholy.Project (assetPath, checkAssetFiles, checkAssets, loadProject, validateProject)
import qualified Unholy.Resource as Resource
import Unholy.Types

data GameContext = GameContext {assetRoot :: FilePath, gamePhysics :: Physics, gameAudio :: Maybe Audio.Engine, gameResources :: Resource.Scope, saveRoot :: FilePath, enteringLevel :: Bool, levelData :: Maybe Value}

acquireGameResource :: GameContext -> IO a -> (a -> IO ()) -> IO a
acquireGameResource ctx = Resource.acquire (gameResources ctx)

data Step = Step {deltaTime :: !Float, elapsedTime :: !Float} deriving (Eq, Show)

type System state = GameContext -> Step -> Input -> state -> Project -> IO (state, Project, [Effect])

data Game state = Game
  { identifier :: String,
    requiredAssets :: [FilePath],
    components :: [SomeComponent],
    validateScene :: Project -> Either String (),
    initialize :: GameContext -> Project -> IO (state, Project, [Effect]),
    beforePhysics :: System state,
    afterPhysics :: [CollisionEvent] -> System state
  }

type FrameSystem state = GameContext -> Float -> ActionFrame -> state -> Project -> IO (state, Project, [Effect])

data SomeGame = forall state. SomeGame (Game state) (Maybe (FrameSystem state)) [(String, FilePath)]

data RunningGame = forall state. RunningGame (Game state) (Maybe (FrameSystem state)) state

game :: Game state -> SomeGame
game definition = SomeGame definition Nothing []

gameWithInterface :: Game state -> FrameSystem state -> [(String, FilePath)] -> SomeGame
gameWithInterface definition frame levels = SomeGame definition (Just frame) levels

simpleGame :: String -> state -> Game state
simpleGame key initialState =
  Game
    key
    []
    []
    (const (Right ()))
    (\_ p -> pure (initialState, p, []))
    (\_ _ _ state p -> pure (state, p, []))
    (\_ _ _ _ state p -> pure (state, p, []))

gameComponents :: [SomeGame] -> Project -> Either String [SomeComponent]
gameComponents registry p = fmap (maybe [] (\(SomeGame definition _ _) -> components definition)) (findGame registry p)

validateGame :: [SomeGame] -> Project -> Either String ()
validateGame registry p = do
  validateProject p
  _ <- gameLevels registry p
  selected <- findGame registry p
  case selected of
    Nothing -> Right ()
    Just (SomeGame definition _ _) -> validateComponents (components definition) (world p) >> validateScene definition p

gameIdentifier :: SomeGame -> String
gameIdentifier (SomeGame definition _ _) = identifier definition

findGame :: [SomeGame] -> Project -> Either String (Maybe SomeGame)
findGame registry p = case haskellGame p of
  Nothing -> Right Nothing
  Just key -> case filter ((== key) . gameIdentifier) registry of
    [definition] -> Right (Just definition)
    [] -> Left ("Haskell game is not linked into this application: " ++ key ++ ". Rebuild the application with its Game registration.")
    _ -> Left ("Duplicate Haskell game registration: " ++ key)

data Panel = Panel {panelTitle :: String, panelBody :: String, panelButtons :: [(String, String)]} deriving (Eq, Show)

data Presentation = Presentation {gamePanel :: Maybe Panel, gamePrompt :: String} deriving (Eq, Show)

emptyPresentation :: Presentation
emptyPresentation = Presentation Nothing ""

-- Optional transient game-owned data makes scene handoff independent of disk saves.
data Command = LoadLevel String | LoadLevelWith String Value | ExitGame deriving (Eq, Show)

data Effect
  = Log String
  | HUD String
  | Sound FilePath Float Float Bool
  | Teleport Entity
  | Present Presentation
  | PauseSimulation Bool
  | Request Command
  deriving (Eq, Show)

startGame :: GameContext -> SomeGame -> Project -> IO (RunningGame, Project, [Effect])
startGame ctx (SomeGame definition frame _) p = hook definition "initialize" $ do
  (state, next, effects) <- initialize definition ctx p
  either (ioError . userError) pure (validateComponents (components definition) (world next))
  validate next
  pure (RunningGame definition frame state, next, effects)

tickGame :: Bool -> [CollisionEvent] -> GameContext -> Step -> Input -> RunningGame -> Project -> IO (RunningGame, Project, [Effect])
tickGame after events ctx dt input (RunningGame definition frame state) p = hook definition (if after then "afterPhysics" else "beforePhysics") $ do
  (nextState, next, effects) <- (if after then afterPhysics definition events else beforePhysics definition) ctx dt input state p
  either (ioError . userError) pure (validateComponents (components definition) (world next))
  validate next
  pure (RunningGame definition frame nextState, next, effects)

validate :: Project -> IO ()
validate p = either (ioError . userError) pure (validateProject p)

hook :: Game state -> String -> IO a -> IO a
hook definition label operation = do
  result <- trySync operation
  either (ioError . userError . (("Game " ++ identifier definition ++ " / " ++ label ++ ": ") ++) . show) pure result

applyEffects :: GameContext -> [Effect] -> IO ([String], String)
applyEffects ctx = foldM apply ([], "")
  where
    apply (logs, hud) effect = case effect of
      Log message -> evaluate (force message) >> pure (logs ++ [message], hud)
      HUD message -> evaluate (force message) >> pure (logs, message)
      Teleport e -> requestTeleport (gamePhysics ctx) e >> pure (logs, hud)
      Sound path volume pitch looping -> do
        either (ioError . userError) pure (assetPath path)
        Audio.play (gameAudio ctx) (assetRoot ctx </> path) volume pitch looping
        pure (logs, hud)
      _ -> pure (logs, hud)

checkGameAssets :: [SomeGame] -> FilePath -> Project -> IO [String]
gameAssetReferences :: [SomeGame] -> Project -> Either String [FilePath]
gameAssetReferences registry p = do
  selected <- findGame registry p
  levels <- gameLevels registry p
  pure (maybe [] (\(SomeGame definition _ _) -> requiredAssets definition) selected ++ map snd levels)
checkGameAssets registry root p = case gameAssetReferences registry p of
  Left err -> pure [err]
  Right paths -> do
    missing <- checkAssetFiles root paths
    levels <- either (const (pure [])) pure (gameLevels registry p)
    errors <- forM levels $ \(key, path) -> case assetPath path of
      Left err -> pure [err]
      Right _ | null key || takeDirectory path /= "." -> pure ["Levels need a name and a manifest beside the entry project"]
      Right _ -> do
        decoded <- loadProject (root </> path)
        case decoded of
          Left err -> pure ["Level " ++ key ++ ": " ++ err]
          Right level -> do
            let valid = if haskellGame level /= haskellGame p then Left "Level game differs from entry game" else validateGame registry level
            assets <- checkAssets root level
            pure (either (\err -> ["Level " ++ key ++ ": " ++ err]) (const []) valid ++ assets)
    pure (missing ++ concat errors)

interactiveGame :: RunningGame -> Bool
interactiveGame (RunningGame _ frame _) = case frame of Nothing -> False; Just _ -> True

gameLevels :: [SomeGame] -> Project -> Either String [(String, FilePath)]
gameLevels registry p = do
  levels <- fmap (maybe [] (\(SomeGame _ _ paths) -> paths)) (findGame registry p)
  when (length (nub (map fst levels)) /= length levels) (Left "Level keys must be unique")
  forM_ levels $ \(key, path) -> do
    assetPath path
    when
      (null key || takeDirectory path /= "." || not (".hj.json" `isSuffixOf` path))
      (Left "Levels need a unique name and a .hj.json manifest beside the entry project")
  pure levels

frameGame :: GameContext -> Float -> ActionFrame -> RunningGame -> Project -> IO (RunningGame, Project, [Effect])
frameGame ctx dt input running@(RunningGame definition frame state) p = case frame of
  Nothing -> pure (running, p, [])
  Just update -> hook definition "frameUpdate" $ do
    (nextState, next, effects) <- update ctx dt input state p
    either (ioError . userError) pure (validateComponents (components definition) (world next))
    validate next
    pure (RunningGame definition frame nextState, next, effects)
