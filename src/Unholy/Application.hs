module Unholy.Application (runApplication) where

import Unholy.Game (SomeGame,findGame,checkGameAssets,gameAssetReferences)
import qualified Paths_unholy as Paths
import Unholy.Types
import Unholy.Project
import Unholy.Script
import qualified Unholy.Native as N
import Unholy.Editor
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import GHC.IO.Encoding (setLocaleEncoding,utf8)
import Control.Monad (when,foldM)
import Control.Exception (finally)
import Control.Concurrent (runInBoundThread)
import Text.Read (readMaybe)

runApplication :: [SomeGame] -> IO ()
runApplication registry = runInBoundThread $ do
  setLocaleEncoding utf8
  args<-getArgs
  when ("--help" `elem` args) $ do
    putStrLn "Unholy 0.1 / Haskell 3D engine\n\n  unholy [PROJECT]                     Open editor (default: material-lab)\n  unholy --play PROJECT                Run standalone player\n  unholy --check PROJECT               Validate scene and assets\n  unholy --build PROJECT               Export runnable desktop folder\n  unholy --simulate STEPS PROJECT      Run headless Haskell/Lua + Bullet steps\n\n  --engine-assets DIR              Engine asset directory\n  --replay FILE                    Play raylib automation events\n  --frames N --capture FILE.png    Bounded visual smoke run"
    exitSuccess
  let positional=removeOptions args
  path<-case positional of
    p:_->makeAbsolute p
    _->do engine<-engineAssets args;makeAbsolute (takeDirectory engine </> "projects" </> "material-lab" </> projectFile)
  loaded<-loadProject path
  p<-either failWith pure loaded
  _<-either failWith pure (findGame registry p)
  sceneAssets<-checkAssets (takeDirectory path) p
  gameAssets<-checkGameAssets registry (takeDirectory path) p
  let missing=sceneAssets++gameAssets
  if "--check" `elem` args then do
    if null missing then putStrLn ("Valid project: "++projectName p++" / "++show (length (renderables (world p)))++" renderables") else failWith (unlines missing)
  else if "--build" `elem` args then do
    when (not (null missing)) (failWith (unlines missing))
    engine<-engineAssets args
    refs<-either failWith pure (gameAssetReferences registry p)
    built<-buildProjectWithAssets refs (takeDirectory path) engine p
    either failWith (putStrLn . ("Built: "++)) built
  else case option "--simulate" args >>= readMaybe of
    Just steps->do
      when (not (null missing)) (failWith (unlines missing))
      (runtime,started,msg)<-startRuntimeWith registry Nothing (takeDirectory path) p
      mapM_ putStrLn msg
      (_,final)<-foldM (\(r,current) _->do
        (nextR,nextP,messages,status)<-stepRuntime (1/120) [0,0,0,0] r current
        mapM_ putStrLn messages
        when (not (null status)) (pure ())
        pure (nextR,nextP)) (runtime,started) [1..steps::Int] `finally` stopRuntime runtime
      either failWith pure (validateProject final)
      putStrLn ("Simulated "++show steps++" steps: "++projectName p)
    Nothing->do
      engine<-engineAssets args
      N.withWindow ("Unholy / "++projectName p) engine $ \window -> do
        case option "--replay" args of
          Nothing->pure ()
          Just replay->do valid<-N.loadReplay window replay;when (not valid) (failWith "No raylib automation events loaded")
        let limit=option "--frames" args >>= readMaybe
            capture=option "--capture" args
        if "--play" `elem` args then runPlayer window registry engine (takeDirectory path) p limit capture
          else runEditor window registry engine path p limit capture

failWith :: String -> IO a
failWith message = putStrLn message >> exitFailure
option :: String -> [String] -> Maybe String
option _ [] = Nothing
option k (a:b:rest) | a==k=Just b | otherwise=option k (b:rest)
option _ [_] = Nothing
removeOptions :: [String] -> [String]
removeOptions []=[]
removeOptions (a:rest) | a `elem` ["--engine-assets","--frames","--capture","--simulate","--replay"] = removeOptions (drop 1 rest)
                       | a `elem` ["--play","--check","--build"] = removeOptions rest
                       | otherwise=a:removeOptions rest
findEngineAssets :: IO FilePath
findEngineAssets = do
  exe<-getExecutablePath
  cwd<-getCurrentDirectory
  installed<-Paths.getDataFileName "assets"
  let ancestors=take 8 (iterate takeDirectory (takeDirectory exe))
      roots=[takeDirectory exe </> "engine",cwd </> "assets"]++map (</> "assets") ancestors++[installed]
  findValid roots
  where
    findValid []=failWith "Cannot find engine shaders. Launch from the repository or supply --engine-assets DIR."
    findValid (p:ps)=do exists<-doesFileExist (p </> "shaders" </> "pbr.fs");if exists then makeAbsolute p else findValid ps

engineAssets :: [String] -> IO FilePath
engineAssets args=maybe findEngineAssets makeAbsolute (option "--engine-assets" args)
