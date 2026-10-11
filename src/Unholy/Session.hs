-- One stable owner spans scene changes, including failure and editor Stop.
module Unholy.Session
  ( Session,
    startSession,
    closeSession,
    advanceSession,
    singleStepSession,
    sessionPresentation,
    sessionInteractive,
    sessionQuit,
    sessionGeneration,
    sessionProject,
    drawSessionUI,
  )
where

import Control.Exception (mask, mask_, onException)
import Control.Monad (foldM, forM_, when)
import Data.Aeson (Value)
import Data.IORef
import qualified Data.Set as Set
import System.FilePath
import qualified Unholy.Audio as Audio
import Unholy.Exception (trySync)
import Unholy.Game
import Unholy.Input
import qualified Unholy.Native as N
import Unholy.Project (assetPath, checkAssets, loadProject)
import Unholy.Script
import Unholy.Types

data Session = Session
  { active :: IORef (Maybe (Runtime, Project)),
    visible :: IORef Project,
    status :: IORef String,
    remainder :: IORef Float,
    previous :: IORef FrameInput,
    selection :: IORef Int,
    clicks :: IORef [String],
    quitting :: IORef Bool,
    generation :: IORef Int,
    failedTarget :: IORef (Maybe (String, Maybe Value)),
    registry :: [SomeGame],
    sound :: Maybe Audio.Engine,
    root :: FilePath,
    saves :: FilePath
  }

startSession :: String -> [SomeGame] -> Maybe Audio.Engine -> FilePath -> Project -> IO (Session, Project, [String])
startSession namespace games audio base p = mask_ $ do
  saveDir <- defaultSaveRoot namespace p
  s <-
    Session
      <$> newIORef Nothing
      <*> newIORef p
      <*> newIORef ""
      <*> newIORef 0
      <*> newIORef emptyFrame
      <*> newIORef 0
      <*> newIORef []
      <*> newIORef False
      <*> newIORef 0
      <*> newIORef Nothing
      <*> pure games
      <*> pure audio
      <*> pure base
      <*> pure saveDir
  (r, next, logs) <- startRuntimeIn games audio base saveDir False p
  writeIORef (active s) (Just (r, next)) `onException` stopRuntime r
  writeIORef (visible s) next
  pure (s, next, logs)

closeSession :: Session -> IO ()
closeSession s = mask_ $ do
  current <- atomicModifyIORef' (active s) (\a -> (Nothing, a))
  forM_ current (stopRuntime . fst)

sessionQuit :: Session -> IO Bool
sessionQuit = readIORef . quitting

sessionGeneration :: Session -> IO Int
sessionGeneration = readIORef . generation

sessionProject :: Session -> IO Project
sessionProject = readIORef . visible

sessionInteractive :: Session -> IO Bool
sessionInteractive s = maybe False (runtimeInteractive . fst) <$> readIORef (active s)

sessionPresentation :: Session -> IO Presentation
sessionPresentation s = do
  current <- readIORef (active s)
  case current of
    Just (r, _) -> runtimePresentation r
    Nothing -> pure (Presentation (Just (Panel "Scene could not start" "Retry the transition or exit. Editor Stop restores your scene." [("retry-transition", "Retry"), ("exit-session", "Exit")])) "")

advanceSession :: Float -> FrameInput -> Session -> IO (Project, [String], String)
advanceSession dt input s = do
  before <- readIORef (previous s)
  writeIORef (previous s) input
  ui <- sessionPresentation s
  index <- readIORef (selection s)
  pending <- atomicModifyIORef' (clicks s) (\xs -> ([], xs))
  let raw = actionFrame defaultBindings dt before input
      buttons = maybe [] panelButtons (gamePanel ui)
      count = length buttons
      nextIndex = if count == 0 then 0 else (index + fromEnum (actionPressed "down" raw) - fromEnum (actionPressed "up" raw)) `mod` count
      confirm = [fst (buttons !! nextIndex) | count > 0, actionPressed "confirm" raw]
      activated = filter (`elem` map fst buttons) (pending ++ confirm)
      actions = raw {activatedButtons = activated, actionPresseds = if count > 0 then Set.delete "jump" (Set.delete "interact" (actionPresseds raw)) else actionPresseds raw}
  writeIORef (selection s) nextIndex
  current <- readIORef (active s)
  messages <- case current of
    Nothing -> do
      when ("exit-session" `elem` activated) (writeIORef (quitting s) True)
      targetKey <- readIORef (failedTarget s)
      if "retry-transition" `elem` activated then maybe (pure []) (uncurry (transition s)) targetKey else pure []
    Just (runtime, p) -> do
      (r, next, frameLogs, frameHud) <- frameRuntime dt actions runtime p
      writeIORef (active s) (Just (r, next))
      rememberHUD s frameHud
      pausedNow <- runtimePaused r
      acc <- readIORef (remainder s)
      (final, drawn, tickLogs, left) <- if pausedNow then pure (r, next, [], 0) else steps 0 (min 0.1 (acc + dt)) r next []
      writeIORef (active s) (Just (final, drawn))
      writeIORef (visible s) drawn
      writeIORef (remainder s) left
      commands <- takeCommands final
      travelLogs <-
        foldM
          ( \logs command -> case command of
              ExitGame -> writeIORef (quitting s) True >> pure logs
              LoadLevel key -> (logs ++) <$> transition s key Nothing
              LoadLevelWith key payload -> (logs ++) <$> transition s key (Just payload)
          )
          []
          (take 1 commands)
      pure (frameLogs ++ tickLogs ++ travelLogs)
  p <- readIORef (visible s)
  hud <- readIORef (status s)
  pure (p, messages, hud)
  where
    steps :: Int -> Float -> Runtime -> Project -> [String] -> IO (Runtime, Project, [String], Float)
    steps count remaining r p logs
      | count >= 12 || remaining < 1 / 120 = pure (r, p, logs, remaining)
      | otherwise = do
          (next, project, msg, hud) <- stepRuntime (1 / 120) (inputValues (gameInput False input)) r p
          rememberHUD s hud
          pureStep <- runtimePaused next
          if pureStep then pure (next, project, logs ++ msg, 0) else steps (count + 1) (remaining - 1 / 120) next project (logs ++ msg)

rememberHUD :: Session -> String -> IO ()
rememberHUD s value = when (not (null value)) (writeIORef (status s) value)

singleStepSession :: Input -> Session -> IO (Project, [String], String)
singleStepSession controls s = do
  current <- readIORef (active s)
  case current of
    Nothing -> (,,) <$> readIORef (visible s) <*> pure [] <*> readIORef (status s)
    Just (r, p) -> do
      (next, project, logs, hud) <- stepRuntime (1 / 120) (inputValues controls) r p
      writeIORef (active s) (Just (next, project))
      writeIORef (visible s) project
      rememberHUD s hud
      pure (project, logs, hud)

transition :: Session -> String -> Maybe Value -> IO [String]
transition s key payload = mask $ \restore -> do
  p <- readIORef (visible s)
  -- Preflight leaves the current scene alive. Native worlds never overlap:
  -- old Runtime teardown owns all voices on the shared audio device.
  prepared <- trySync $ restore $ do
    levels <- either (ioError . userError) pure (gameLevels (registry s) p)
    file <- maybe (ioError (userError ("Unknown level: " ++ key))) pure (lookup key levels)
    either (ioError . userError) pure (assetPath file)
    when (takeDirectory file /= ".") (ioError (userError "Level files must share the entry project's asset root"))
    targetProject <- loadProject (root s </> file) >>= either (ioError . userError) pure
    when (haskellGame targetProject /= haskellGame p) (ioError (userError "A level must use the same registered game"))
    either (ioError . userError) pure (validateGame (registry s) targetProject)
    missing <- (++) <$> checkAssets (root s) targetProject <*> checkGameAssets (registry s) (root s) targetProject
    when (not (null missing)) (ioError (userError (unlines missing)))
    pure targetProject
  case prepared of
    Left err -> pure ["Level preflight failed: " ++ show err]
    Right targetProject -> do
      writeIORef (failedTarget s) (Just (key, payload))
      closeSession s
      result <- trySync $ startRuntimeForLevel (registry s) (sound s) (root s) (saves s) True payload targetProject
      writeIORef (remainder s) 0
      writeIORef (clicks s) []
      writeIORef (selection s) 0
      writeIORef (status s) ""
      case result of
        Left err -> pure ["Level initialization failed: " ++ show err]
        Right (r, next, logs) -> do
          writeIORef (active s) (Just (r, next)) `onException` stopRuntime r
          writeIORef (visible s) next
          writeIORef (failedTarget s) Nothing
          modifyIORef' (generation s) (+ 1)
          pure (("Entered level: " ++ key) : logs)

-- Identified buttons share mouse and keyboard/controller activation policy.
drawSessionUI :: N.Window -> (Float, Float, Float, Float) -> Session -> IO ()
drawSessionUI window (x, y, w, h) s = do
  ui <- sessionPresentation s
  index <- readIORef (selection s)
  N.clip window x y w h
  when (not (null (gamePrompt ui))) $ do
    N.rect window (x + 18) (y + h - 85) (w - 36) 66 0
    N.text window (x + 32) (y + h - 69) 17 (gamePrompt ui) 0
  forM_ (gamePanel ui) $ \panel -> do
    let width = min 480 (w - 40); left = x + (w - width) / 2; top = y + max 20 ((h - 350) / 2)
    N.rect window left top width 350 0
    N.rect window left top width 4 2
    N.text window (left + 24) (top + 22) 29 (panelTitle panel) 2
    N.text window (left + 24) (top + 69) 16 (panelBody panel) 0
    forM_ (zip [0 :: Int ..] (panelButtons panel)) $ \(i, (key, label)) -> do
      pressed <- N.button window (left + 24) (top + 145 + fromIntegral i * 45) (width - 48) 37 label (if i == index then 3 else 0)
      when pressed (modifyIORef' (clicks s) (++ [key]))
    N.text window (left + 24) (top + 325) 12 "Arrows / D-pad select  |  Enter / A confirm" 1
  N.unclip window
