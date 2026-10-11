module ReliabilitySpec (projectReliability) where

import Unholy
import Unholy.Project (checkAssetFiles,checkAssets,assetPath,copyTree)
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
        valid {environment=defaultEnvironment {sunColor=V3 (1/0) 0 0}},valid {environment=defaultEnvironment {environmentMap="../escape.hdr"}},valid {environment=defaultEnvironment {environmentIntensity=(-1)}},valid {environment=defaultEnvironment {environmentMap="bad.png"}},valid {physicsSettings=defaultPhysicsSettings {supportNormal=2}}]
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
  copyFile "projects/material-gallery/assets/studio.hdr" (root </> "studio.hdr")
  let (e,w)=spawn "Script" emptyWorld
      p=valid {world=w {scripts=IM.singleton e "logic.lua"},environment=defaultEnvironment {environmentMap="studio.hdr",environmentIntensity=1.7,environmentRotation=72}}
  built<-buildProjectWithAssets ["nested/game.dat"] root "assets" p
  output<-either (ioError.userError) pure built
  forM_ ["logic.lua","nested/game.dat","assets/runtime-only.dat","studio.hdr"] $ \ref->do
    exists<-doesFileExist (output </> ref)
    assert ("Export contains "++ref) exists
  staged<-loadProject (output </> projectFile)
  assert "Export scene round-trips" (staged==Right p)
  writeFile (output </> "retained.marker") "previous export"
  createDirectoryIfMissing True (root </> "bad-engine/shaders")
  failure<-buildProject root (root </> "bad-engine") p
  retained<-doesFileExist (output </> "retained.marker")
  assert "Staging failure retains previous export" (either (const True) (const False) failure && retained)
  removeFile (root </> "studio.hdr")
  missingEnvironment<-checkAssets root p
  assert "Project-level HDR references are checked" (not (null missingEnvironment))
  writeFile (root </> "studio.hdr") "not an HDR image"
  corrupt<-checkAssets root p
  assert "Corrupt HDR fails headless preflight" (not (null corrupt))
  failedHDR<-buildProject root "assets" p
  assert "Corrupt HDR cannot replace the previous export" (either (const True) (const False) failedHDR)
  copyFile "projects/material-gallery/assets/studio.hdr" (root </> "studio.hdr")
  base<-canonicalizePath root
  createDirectoryLink (base </> "nested") (root </> "linked")
  errors<-checkAssetFiles root ["linked/game.dat"] `finally` removeDirectoryLink (root </> "linked")
  assert "Symlinked asset ancestor rejected" (not (null errors))
  copyTree "projects/material-gallery/assets" (root </> "import")
  let (model,w2)=spawnMesh "Imported" "import/cards.gltf" (world p)
      imported=p {world=w2 {meshes=IM.adjust (\mesh->mesh {useImportedMaterials=True}) model (meshes w2)}}
  complete<-checkAssets root imported
  assert "glTF headless material preflight" (null complete)
  removeFile (root </> "import/directional-normal.png")
  absent<-checkAssets root imported
  assert "glTF image dependency missing is detected headlessly" (not (null absent))
  copyFile "projects/material-gallery/assets/directional-normal.png" (root </> "import/directional-normal.png")
  exported<-buildProject root "assets" imported >>= either (ioError.userError) pure
  forM_ ["import/cards.bin","import/gray.png","import/metal-rough.png","import/directional-normal.png"] $ \ref->doesFileExist (exported </> ref) >>= assert ("Exported importer dependency missing: "++ref)
  writeFile (root </> "import/unsupported.gltf") "{\"materials\":[{\"alphaMode\":\"BLEND\"}]}"
  let unsupported=imported {world=(world imported) {meshes=IM.adjust (\mesh->mesh {geometry="import/unsupported.gltf"}) model (meshes (world imported))}}
  rejected<-checkAssets root unsupported
  assert "Unsupported imported materials fail before export" (not (null rejected))
  let legacy=unsupported {world=(world unsupported) {meshes=IM.adjust (\mesh->mesh {useImportedMaterials=False}) model (meshes (world unsupported))}}
  retainedGeometry<-checkAssets root legacy
  assert "Legacy entity override does not reject unsupported imported materials" (null retainedGeometry)
  writeFile (root </> "import/unsupported.gltf") "{\"samplers\":[{\"wrapS\":33071}]}"
  samplerErrors<-checkAssets root unsupported
  assert "Unsupported texture samplers fail explicitly" (not (null samplerErrors))
