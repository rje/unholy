module Main where

import Control.Exception (bracket)
import Control.Monad (foldM, forM_, unless)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (isInfixOf, sort)
import LastCue
import System.Directory
import System.Environment (lookupEnv, setEnv)
import Unholy
import qualified Unholy.ECS as ECS
import Unholy.Session

assert :: String -> Bool -> IO ()
assert label okay = unless okay (ioError (userError label))

playerPosition :: Project -> V3
playerPosition p = position (transforms (world p) IM.! head [e | (e, key) <- ECS.query (world p) roles, key == "player"])
  where
    roles = either error id (readComponents roleComponent (world p))

frame :: [Int] -> FrameInput
frame keys = emptyFrame {heldKeys = IS.fromList keys}

data Harness = Harness {runtime :: Session, replayFrames :: IORef [[Int]]}

tick :: Harness -> [Int] -> IO Project
tick harness keys = do
  modifyIORef' (replayFrames harness) (sort keys :)
  (p, logs, _) <- advanceSession (1 / 60) (frame keys) (runtime harness)
  mapM_ putStrLn logs
  pure p

idle :: Harness -> Int -> IO Project
idle session count = foldM (\_ _ -> tick session []) sampleProject [1 .. count]

walk :: Harness -> Float -> Float -> IO Project
walk session targetX targetZ = do
  original <- sessionGeneration (runtime session)
  go original (0 :: Int)
  where
    go original count = do
      p <- sessionProject (runtime session)
      changed <- sessionGeneration (runtime session)
      ui <- sessionPresentation (runtime session)
      let V3 x _ z = playerPosition p
          keys =
            [if targetX > x then 68 else 65 | abs (targetX - x) > 0.13]
              ++ [if targetZ > z then 83 else 87 | abs (targetZ - z) > 0.13]
              ++ [69 | count `mod` 45 == 0]
      if changed /= original || fmap panelTitle (gamePanel ui) == Just "CURTAIN CALL" || abs (targetX - x) + abs (targetZ - z) < 0.28
        then pure p
        else
          if count > 1400
            then ioError (userError ("Route stalled at " ++ show (playerPosition p) ++ " aiming for " ++ show (targetX, targetZ)))
            else tick session keys >> go original (count + 1)

main :: IO ()
main = do
  local <- doesFileExist "projects/last-cue/project.hj.json"
  unless local (setCurrentDirectory "../..")
  setEnv "UNHOLY_SAVE_DIR" "test-output/last-cue-saves"
  exists <- doesDirectoryExist "test-output/last-cue-saves"
  if exists then removePathForcibly "test-output/last-cue-saves" else pure ()
  bracket (startSession "test" [lastCue] Nothing "projects/last-cue" sampleProject) (closeSession . first) $ \(owned, _, _) -> do
    session <- Harness owned <$> newIORef []
    ui <- sessionPresentation (runtime session)
    assert "Missing title menu" (fmap panelTitle (gamePanel ui) == Just "LAST CUE")
    _ <- tick session [257]
    _ <- idle session 20
    _ <- walk session (-4) 5
    _ <- walk session 4 1
    collected <- idle session 1
    roles <- either (ioError . userError) pure (readComponents roleComponent (world collected))
    assert "Cue pickup did not remove its ECS assembly" (all (\(_, key) -> key /= "cue-a" && key /= "cue-b") (ECS.query (world collected) roles))
    _ <- walk session 0 (-3)
    _ <- tick session [69]
    _ <- idle session 2
    _ <- walk session 2 (-3)
    _ <- walk session 2 (-7.8)
    _ <- walk session 0 (-7.8)
    let ride count = do
          p <- tick session []
          let V3 _ y _ = playerPosition p
          if y > 3.95 then pure p else if count > 600 then ioError (userError "Scenery lift did not carry stagehand") else ride (count + 1)
    _ <- ride (0 :: Int)
    _ <- walk session 0 (-12.3)
    generation <- sessionGeneration (runtime session)
    assert "Upper exit did not transition to the second act" (generation == 1)
    _ <- idle session 2
    _ <- walk session (-4) 5
    _ <- walk session 4 1
    _ <- walk session 0 (-3)
    _ <- tick session [69]
    _ <- idle session 2
    _ <- walk session 2 (-3)
    _ <- walk session 2 (-7.8)
    _ <- walk session 0 (-7.8)
    _ <- ride (0 :: Int)
    _ <- walk session 0 (-12.3)
    uiComplete <- sessionPresentation (runtime session)
    assert "Two-act performance did not complete" (fmap panelTitle (gamePanel uiComplete) == Just "CURTAIN CALL")
    output <- lookupEnv "UNHOLY_RECORD_ROUTE"
    forM_ output $ \path -> do
      frames <- reverse <$> readIORef (replayFrames session)
      let events = concat [map (event n 1) (filter (`notElem` now) old) ++ map (event n 2) (filter (`notElem` old) now) | (n, (old, now)) <- zip [10 :: Int ..] (zip ([] : frames) frames)]
          event n kind key = "e " ++ unwords (map show [n, kind, key, 0, 0, 0])
      writeFile path ("# Last Cue deterministic 60 Hz route\nc " ++ show (length events) ++ "\n" ++ unlines events)
      writeFile (path ++ ".frames") (show (length frames + 30))
    putStrLn "PASS actual capsule movement, two cue pairs, winches, moving lifts, sentinel and both-act completion"
  bracket (startSession "test" [lastCue] Nothing "projects/last-cue" sampleProject) (closeSession . first) $ \(owned, _, _) -> do
    session <- Harness owned <$> newIORef []
    _ <- tick session [264]
    _ <- tick session []
    _ <- tick session [257]
    _ <- idle session 2
    generation <- sessionGeneration (runtime session)
    assert "Continue failed to load the saved second act" (generation == 1)
    ui <- sessionPresentation (runtime session)
    assert "Continue left gameplay paused" (gamePanel ui == Nothing)
    _ <- tick session [256]
    paused <- idle session 10
    again <- idle session 10
    assert "Pause advanced physics" (world paused == world again)
    menu <- sessionPresentation (runtime session)
    assert "Pause menu missing" (fmap panelTitle (gamePanel menu) == Just "INTERMISSION")
    putStrLn "PASS checkpoint reload across a fresh session, keyboard menu focus and paused simulation"
    _ <- tick session [257]
    _ <- idle session 360
    caught <- sessionPresentation owned
    assert "Sentinel never caught stationary player" (fmap panelTitle (gamePanel caught) == Just "MISSED YOUR CUE")
    _ <- tick session [257]
    _ <- idle session 2
    retried <- sessionPresentation owned
    assert "Caught Retry did not restore checkpoint" (gamePanel retried == Nothing)
    putStrLn "PASS line-of-sight chase, capture and retry at the durable stage mark"
  writeFile "test-output/last-cue-unwritable" "retained marker"
  setEnv "UNHOLY_SAVE_DIR" "test-output/last-cue-unwritable"
  bracket (startSession "test" [lastCue] Nothing "projects/last-cue" sampleProject) (closeSession . first) $ \(owned, _, _) -> do
    session <- Harness owned <$> newIORef []
    _ <- tick session [257]
    ui <- sessionPresentation owned
    assert "Save failure prevented New Game" (gamePanel ui == Nothing)
    assert "Save warning is invisible" ("Progress is in memory" `isInfixOf` gamePrompt ui)
    _ <- walk session (-4) 5
    _ <- idle session 2
    _ <- tick session [256]
    _ <- tick session []
    _ <- tick session [264]
    _ <- tick session []
    _ <- tick session [264]
    _ <- tick session []
    _ <- tick session [257]
    _ <- idle session 2
    generation <- sessionGeneration owned
    assert "In-memory retry depended on writable storage" (generation == 1)
    readFile "test-output/last-cue-unwritable" >>= assert "Failed save damaged prior storage" . (== "retained marker")
    putStrLn "PASS unsaved play, visible storage warning and in-memory transition progress"
  where
    first (a, _, _) = a
