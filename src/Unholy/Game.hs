{-# LANGUAGE ExistentialQuantification #-}
-- A game keeps its own statically typed state. Only the registration boundary
-- hides that type; there is no Dynamic component store or scripting ABI.
module Unholy.Game
  ( Game(..), SomeGame, game, gameIdentifier, GameContext(..), Step(..), System
  , Effect(..), checkGameAssets, RunningGame, startGame, tickGame, applyEffects, findGame ) where

import Unholy.Types
import Unholy.Input
import Unholy.Physics
import qualified Unholy.Audio as Audio
import Unholy.Project (assetPath)
import Control.Monad (foldM, filterM)
import System.Directory (doesFileExist)
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
startGame ctx (SomeGame definition) p=do
  (state,next,effects)<-initialize definition ctx p
  validate next
  pure (RunningGame definition state,next,effects)
tickGame :: Bool -> [CollisionEvent] -> GameContext -> Step -> Input -> RunningGame -> Project -> IO (RunningGame,Project,[Effect])
tickGame after events ctx dt input (RunningGame definition state) p=do
  (nextState,next,effects)<-(if after then afterPhysics definition events else beforePhysics definition) ctx dt input state p
  validate next
  pure (RunningGame definition nextState,next,effects)
validate :: Project -> IO ()
validate p=either (ioError.userError) pure (validateWorld (world p))
applyEffects :: GameContext -> [Effect] -> IO ([String],String)
applyEffects ctx=foldM apply ([],"")
  where
    apply (logs,hud) effect=case effect of
      Log message->pure (logs++[message],hud)
      HUD message->pure (logs,message)
      Teleport e->requestTeleport (gamePhysics ctx) e >> pure (logs,hud)
      Sound path volume pitch looping->do
        either (ioError.userError) pure (assetPath path)
        Audio.play (gameAudio ctx) (assetRoot ctx </> path) volume pitch looping
        pure (logs,hud)

checkGameAssets :: [SomeGame] -> FilePath -> Project -> IO [String]
checkGameAssets registry root p=case findGame registry p of
  Left err->pure [err]
  Right Nothing->pure []
  Right (Just (SomeGame definition))->do
    let paths=requiredAssets definition
        invalid=[err | path<-paths,Left err<-[assetPath path]]
    missing<-filterM (fmap not . doesFileExist . (root </>)) [path | path<-paths,assetPath path==Right ()]
    pure (invalid++map ("Missing Haskell game asset: "++) missing)
