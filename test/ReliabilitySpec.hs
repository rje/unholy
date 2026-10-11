module ReliabilitySpec (projectReliability) where

import Unholy
import Unholy.Project (checkAssetFiles,assetPath)
import qualified Data.IntMap.Strict as IM
import Control.Monad (unless,forM_)
import System.Directory
import System.FilePath
import Control.Exception (finally)

assert :: String -> Bool -> IO ()
assert message good=unless good (ioError (userError message))

projectReliability :: IO ()
projectReliability=do
  let root="test-output/reliability"
      path=root </> projectFile
      valid=emptyProject "Reliability"
      invalids=[valid {camera=defaultCamera {fov=0}},valid {camera=defaultCamera {eye=V3 (0/0) 0 0}},
        valid {environment=defaultEnvironment {sunColor=V3 (1/0) 0 0}},valid {physicsSettings=defaultPhysicsSettings {supportNormal=2}}]
  createDirectoryIfMissing True root
  saved<-saveProject path valid
  assert "Valid project saves" (saved==Right ())
  forM_ invalids $ \bad->do
    rejected<-saveProject path bad
    assert "Invalid project cannot replace saved scene" (either (const True) (const False) rejected)
    loaded<-loadProject path
    assert "Prior valid scene remains intact" (loaded==Right valid)
  forM_ ["engine/shaders/pbr.fs","./dist/a","project.hj.json","lib/foo","PLAY.COMMAND","Engine/file"] $ \ref->
    assert "Reserved export destinations rejected" (assetPath ref/=Right ())
  writeFile (root </> "logic.lua") "return {}"
  createDirectoryIfMissing True (root </> "nested")
  writeFile (root </> "nested/game.dat") "game-owned data"
  createDirectoryIfMissing True (root </> "assets")
  writeFile (root </> "assets/runtime-only.dat") "Lua-owned data"
  let (e,w)=spawn "Script" emptyWorld
      p=valid {world=w {scripts=IM.singleton e "logic.lua"}}
  built<-buildProjectWithAssets ["nested/game.dat"] root "assets" p
  output<-either (ioError.userError) pure built
  forM_ ["logic.lua","nested/game.dat","assets/runtime-only.dat"] $ \ref->do
    exists<-doesFileExist (output </> ref)
    assert ("Export contains "++ref) exists
  staged<-loadProject (output </> projectFile)
  assert "Export scene round-trips" (staged==Right p)
  writeFile (output </> "retained.marker") "previous export"
  createDirectoryIfMissing True (root </> "bad-engine/shaders")
  failure<-buildProject root (root </> "bad-engine") p
  retained<-doesFileExist (output </> "retained.marker")
  assert "Staging failure retains previous export" (either (const True) (const False) failure && retained)
  base<-canonicalizePath root
  createDirectoryLink (base </> "nested") (root </> "linked")
  errors<-checkAssetFiles root ["linked/game.dat"] `finally` removeDirectoryLink (root </> "linked")
  assert "Symlinked asset ancestor rejected" (not (null errors))
