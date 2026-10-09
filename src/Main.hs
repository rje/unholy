module Main where

import HJ.Types
import HJ.Project
import HJ.Script
import qualified HJ.Native as N
import HJ.Editor
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import GHC.IO.Encoding (setLocaleEncoding,utf8)
import Control.Monad (when,foldM)
import Text.Read (readMaybe)

main :: IO ()
main = do
  setLocaleEncoding utf8
  args<-getArgs
  when ("--help" `elem` args) $ do
    putStrLn "HJ 0.1 / Haskell 3D engine\n\n  hj [PROJECT]                     Open editor (default: material-lab)\n  hj --play PROJECT                Run standalone player\n  hj --check PROJECT               Validate scene and assets\n  hj --build PROJECT               Export runnable desktop folder\n  hj --simulate STEPS PROJECT      Run headless Lua + Bullet steps\n\n  --engine-assets DIR              Engine asset directory\n  --replay FILE                    Play raylib automation events\n  --frames N --capture FILE.png    Bounded visual smoke run"
    exitSuccess
  engine<-case option "--engine-assets" args of Just p->makeAbsolute p;Nothing->findEngineAssets
  let positional=removeOptions args
      defaultProject=takeDirectory engine </> "projects" </> "material-lab" </> projectFile
  path<-makeAbsolute (case positional of p:_->p;_->defaultProject)
  loaded<-loadProject path
  p<-either failWith pure loaded
  missing<-checkAssets (takeDirectory path) p
  if "--check" `elem` args then do
    if null missing then putStrLn ("Valid project: "++projectName p++" / "++show (length (renderables (world p)))++" renderables") else failWith (unlines missing)
  else if "--build" `elem` args then do
    built<-buildProject (takeDirectory path) engine p
    either failWith (putStrLn . ("Built: "++)) built
  else case option "--simulate" args >>= readMaybe of
    Just steps->do
      when (not (null missing)) (failWith (unlines missing))
      (runtime,started,msg)<-startRuntime (takeDirectory path) p
      mapM_ putStrLn msg
      (r,final)<-foldM (\(r,current) _->do
        (nextR,nextP,messages,status)<-stepRuntime (1/120) [0,0,0,0] r current
        mapM_ putStrLn messages
        when (not (null status)) (pure ())
        pure (nextR,nextP)) (runtime,started) [1..steps::Int]
      stopRuntime r
      either failWith pure (validateWorld (world final))
      putStrLn ("Simulated "++show steps++" steps: "++projectName p)
    Nothing->do
      ready<-N.initWindow ("HJ / "++projectName p) engine
      when (not ready) (N.nativeError >>= failWith)
      case option "--replay" args of
        Nothing->pure ()
        Just replay->do valid<-N.loadReplay replay;when (not valid) (failWith "No raylib automation events loaded")
      let limit=option "--frames" args >>= readMaybe
          capture=option "--capture" args
      if "--play" `elem` args then runPlayer engine (takeDirectory path) p limit capture
        else runEditor engine path p limit capture

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
  let ancestors=take 8 (iterate takeDirectory (takeDirectory exe))
      roots=[cwd </> "assets",takeDirectory exe </> "engine"]++map (</> "assets") ancestors
  findValid roots
  where
    findValid []=failWith "Cannot find engine shaders. Launch from the repository or supply --engine-assets DIR."
    findValid (p:ps)=do exists<-doesFileExist (p </> "shaders" </> "pbr.fs");if exists then makeAbsolute p else findValid ps
