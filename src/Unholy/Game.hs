{-# LANGUAGE ExistentialQuantification #-}
-- A game keeps its own statically typed state. Only the registration boundary
-- hides that type; there is no Dynamic component store or scripting ABI.
module Unholy.Game
  ( Game(..), SomeGame, game, gameIdentifier, GameContext(..), Step(..), System
  , Effect(..), checkGameAssets, gameAssetReferences, RunningGame, startGame, tickGame, applyEffects, findGame ) where

import Unholy.Types
import Unholy.Input
import Unholy.Physics
import qualified Unholy.Audio as Audio
import Unholy.Project (assetPath,validateProject,checkAssetFiles)
import Unholy.Exception (trySync)
import Control.Monad (foldM)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import System.FilePath ((</>))

data GameContext = GameContext { assetRoot :: FilePath, gamePhysics :: Physics, gameAudio :: Maybe Audio.Engine }
data Step = Step { deltaTime :: !Float, elapsedTime :: !Float } deriving (Eq,Show)
type System state = GameContext -> Step -> Input -> state -> Project -> IO (state,Project,[Effect])
data Game state = Game
  { identifier :: String, requiredAssets :: [FilePath]
  , initialize :: GameContext -> Project -> IO (state,Project,[Effect])
  , beforePhysics :: System state
  , afterPhysics :: [CollisionEvent] -> System state }
data SomeGame = forall state. SomeGame (Game state)
data RunningGame = forall state. RunningGame (Game state) state
game :: Game state -> SomeGame
game=SomeGame
gameIdentifier :: SomeGame -> String
gameIdentifier (SomeGame definition)=identifier definition
findGame :: [SomeGame] -> Project -> Either String (Maybe SomeGame)
findGame registry p=case haskellGame p of
  Nothing->Right Nothing
  Just key->case filter ((==key).gameIdentifier) registry of
    [definition]->Right (Just definition)
    []->Left ("Haskell game is not linked into this application: "++key++". Rebuild the application with its Game registration.")
    _->Left ("Duplicate Haskell game registration: "++key)

data Effect = Log String | HUD String | Sound FilePath Float Float Bool | Teleport Entity
  deriving (Eq,Show)
startGame :: GameContext -> SomeGame -> Project -> IO (RunningGame,Project,[Effect])
startGame ctx (SomeGame definition) p=hook definition "initialize" $ do
  (state,next,effects)<-initialize definition ctx p
  validate next
  pure (RunningGame definition state,next,effects)
tickGame :: Bool -> [CollisionEvent] -> GameContext -> Step -> Input -> RunningGame -> Project -> IO (RunningGame,Project,[Effect])
tickGame after events ctx dt input (RunningGame definition state) p=hook definition (if after then "afterPhysics" else "beforePhysics") $ do
  (nextState,next,effects)<-(if after then afterPhysics definition events else beforePhysics definition) ctx dt input state p
  validate next
  pure (RunningGame definition nextState,next,effects)
validate :: Project -> IO ()
validate p=either (ioError.userError) pure (validateProject p)
hook :: Game state -> String -> IO a -> IO a
hook definition label operation=do
  result<-trySync operation
  either (ioError.userError . (("Game "++identifier definition++" / "++label++": ")++) . show) pure result
applyEffects :: GameContext -> [Effect] -> IO ([String],String)
applyEffects ctx=foldM apply ([],"")
  where
    apply (logs,hud) effect=case effect of
      Log message->evaluate (force message) >> pure (logs++[message],hud)
      HUD message->evaluate (force message) >> pure (logs,message)
      Teleport e->requestTeleport (gamePhysics ctx) e >> pure (logs,hud)
      Sound path volume pitch looping->do
        either (ioError.userError) pure (assetPath path)
        Audio.play (gameAudio ctx) (assetRoot ctx </> path) volume pitch looping
        pure (logs,hud)

checkGameAssets :: [SomeGame] -> FilePath -> Project -> IO [String]
gameAssetReferences :: [SomeGame] -> Project -> Either String [FilePath]
gameAssetReferences registry p=fmap (maybe [] (\(SomeGame definition)->requiredAssets definition)) (findGame registry p)

checkGameAssets registry root p=case gameAssetReferences registry p of
  Left err->pure [err]
  Right paths->checkAssetFiles root paths
