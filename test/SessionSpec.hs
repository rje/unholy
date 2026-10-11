module SessionSpec (sessionContracts) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, unless, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (isInfixOf)
import System.Directory
import System.FilePath
import Unholy
import Unholy.Checkpoint
import Unholy.Session

assert :: String -> Bool -> IO ()
assert label okay = unless okay (ioError (userError label))

failed :: Either SomeException a -> Bool
failed = either (const True) (const False)

first :: (a, b, c) -> a
first (a, _, _) = a

sessionContracts :: IO ()
sessionContracts = do
  let root = "test-output/session"
      project = (emptyProject "First") {haskellGame = Just "session-spec"}
      second = project {projectName = "Second"}
      held = emptyFrame {heldKeys = IS.singleton 32}
  createDirectoryIfMissing True root
  saveProject (root </> "first.hj.json") project >>= either (ioError . userError) pure
  saveProject (root </> "second.hj.json") second >>= either (ioError . userError) pure
  released <- newIORef ([] :: [String])
  seen <- newIORef ([] :: [Bool])
  ticks <- newIORef (0 :: Int)
  bad <- newIORef False
  let definition =
        (simpleGame "session-spec" ())
          { initialize = \ctx p -> do
              _ <- acquireGameResource ctx (pure (projectName p)) (\label -> modifyIORef' released (++ [label]))
              broken <- readIORef bad
              when (enteringLevel ctx && broken) (ioError (userError "injected level initializer"))
              pure ((), p, [Present (if enteringLevel ctx then emptyPresentation else Presentation (Just (Panel "Start" "" [("next", "Next")])) "")]),
            beforePhysics = \_ _ _ state p -> modifyIORef' ticks (+ 1) >> pure (state, p, [])
          }
      frameHook _ _ actions state p = do
        modifyIORef' seen (++ [actionPressed "jump" actions])
        pure (state, p, [Request (LoadLevel "second") | "next" `elem` activatedButtons actions])
      registered = gameWithInterface definition frameHook [("first", "first.hj.json"), ("second", "second.hj.json")]
  assert "Duplicate levels accepted" (gameLevels [gameWithInterface definition frameHook [("x", "first.hj.json"), ("x", "second.hj.json")]] project /= Right [("x", "first.hj.json"), ("x", "second.hj.json")])
  assert "Unexportable level suffix accepted" (either (const True) (const False) (gameLevels [gameWithInterface definition frameHook [("x", "second.json")]] project))
  bracket (startSession "test" [registered] Nothing root project) (closeSession . first) $ \(session, _, _) -> do
    removeFile (root </> "second.hj.json")
    (_, logs, _) <- advanceSession 0 held session
    assert "Failed preflight did not retain current owner" (any (isInfixOf "preflight failed") logs)
    readIORef released >>= assert "Preflight closed live scene" . null
    saveProject (root </> "second.hj.json") second >>= either (ioError . userError) pure
    _ <- advanceSession 0 emptyFrame session
    writeIORef bad True
    _ <- advanceSession 0 held session
    menu <- sessionPresentation session
    assert "Initialization failure lacks Retry" (fmap panelTitle (gamePanel menu) == Just "Scene could not start")
    readIORef released >>= assert "Transition cleanup ownership" . (== ["First", "Second"])
    writeIORef bad False
    _ <- advanceSession 0 emptyFrame session
    _ <- advanceSession 0 held session
    generation <- sessionGeneration session
    assert "Transition retry failed" (generation == 1)
    writeIORef seen []
    _ <- advanceSession (1 / 30) held session
    readIORef seen >>= assert "Held confirmation leaked into fresh jump after transition" . (== [False])
    readIORef ticks >>= assert "Fixed ticks were not dispatched" . (== 4)
    closeSession session
    closeSession session
  readIORef released >>= assert "Session closed a runtime more than once" . (== ["First", "Second", "Second"])
  -- Laziness must fail inside runtime ownership, before UI rendering.
  forM_ [True, False] $ \atStart -> do
    closed <- newIORef (0 :: Int)
    let payload = Presentation (Just (Panel "Title" "Body" [("button", error "injected lazy button")])) ""
        gameDef =
          (simpleGame "session-spec" ())
            { initialize = \ctx p -> do
                _ <- acquireGameResource ctx (pure ()) (\_ -> modifyIORef' closed (+ 1))
                pure ((), p, [Present payload | atStart])
            }
        faulty = gameWithInterface gameDef (\_ _ s st p -> pure (st, p, [Present payload | not atStart && actionPressed "interact" s])) []
    if atStart
      then do
        result <- try (startSession "test" [faulty] Nothing root project) :: IO (Either SomeException (Session, Project, [String]))
        assert "Lazy startup presentation escaped" (failed result)
      else bracket (startSession "test" [faulty] Nothing root project) (closeSession . first) $ \(session, _, _) -> do
        result <- try (advanceSession 0 emptyFrame {heldKeys = IS.singleton 69} session) :: IO (Either SomeException (Project, [String], String))
        assert "Lazy frame presentation escaped" (failed result)
    readIORef closed >>= assert "Faulty presentation lost runtime cleanup" . (== 1)
  -- Cancellation at an interruptible initializer releases its registered scope.
  ready <- newEmptyMVar
  blocked <- newEmptyMVar
  done <- newEmptyMVar
  closed <- newIORef (0 :: Int)
  let waiting =
        game
          (simpleGame "session-spec" ())
            { initialize = \ctx p -> do
                _ <- acquireGameResource ctx (pure ()) (\_ -> modifyIORef' closed (+ 1))
                putMVar ready ()
                takeMVar blocked
                pure ((), p, [])
            }
  thread <- forkIO $ do
    result <- try (startSession "test" [waiting] Nothing root project) :: IO (Either SomeException (Session, Project, [String]))
    putMVar done result
  takeMVar ready
  killThread thread
  takeMVar done >>= assert "Startup cancellation was swallowed" . failed
  readIORef closed >>= assert "Cancelled initialization leaked" . (== 1)
  let path = root </> "checkpoint.json"
  writeCheckpoint path (7 :: Int) >>= assert "Checkpoint write failed" . (== Right ())
  result <- writeCheckpoint path (error "injected JSON failure" :: Int)
  assert "Checkpoint encoding failure not contained" (either (const True) (const False) result)
  readCheckpoint path >>= assert "Failed save replaced prior checkpoint" . (== Right (Just (7 :: Int)))
  writeFile path "{broken"
  corrupted <- readCheckpoint path :: IO (Either String (Maybe Int))
  assert "Corrupt save accepted" (either (const True) (const False) corrupted)
  let (actor, w) = spawn "Capsule" emptyWorld
      motor = w {bodies = IM.singleton actor defaultBody {bodyShape = Capsule, mass = 1, lockRotation = True}}
  bracket (createPhysics motor) closePhysics $ \physics -> do
    (forward, _, _) <- driveCharacter physics defaultCharacter (pi / 2) False (Input 0 (-1) False False False) actor motor
    (right, _, _) <- driveCharacter physics defaultCharacter (pi / 2) False (Input 1 0 False False False) actor motor
    let V3 fx _ fz = velocity (bodies forward IM.! actor); V3 rx _ rz = velocity (bodies right IM.! actor)
    assert "Orbit-relative motor directions" (fx < (-4) && abs fz < 0.001 && abs rx < 0.001 && rz < (-4))
