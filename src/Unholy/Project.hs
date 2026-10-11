module Unholy.Project where

import Unholy.Types
import Unholy.Transform (resolveWorld)
import Unholy.JSON
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad (forM, forM_, unless, when, foldM)
import Control.Exception (IOException, try, onException)
import System.Directory
import System.FilePath
import System.Environment (getExecutablePath)
import System.Process (readProcess, callProcess)
import System.Info (os)
import Data.Char (isAlphaNum, toLower)
import Data.List (nub, isPrefixOf)
import Text.Read (readMaybe)

projectFile :: FilePath
projectFile = "project.hj.json"

projectJSON :: Project -> JSON
projectJSON p = Object
  [ ("schemaVersion", Number 2), ("name", String (projectName p)), ("haskellGame", maybe Null String (haskellGame p))
  , ("nextId", int (nextId w)), ("entities", Array (map entity (IM.toAscList (names w))))
  , ("camera", Object [("eye",vec (eye c)),("target",vec (target c)),("fov",num (fov c))])
  , ("physics",Object [("gravity",vec (gravity settings)),("supportNormal",num (supportNormal settings)),("supportDistance",num (supportDistance settings))])
  , ("environment", Object [("sunDirection",vec (sunDirection env)),("sunColor",vec (sunColor env)),("sunPower",num (sunPower env)),("ambient",num (ambient env)),("exposure",num (exposure env)),("skyTop",vec (skyTop env)),("skyBottom",vec (skyBottom env)),("fogDensity",num (fogDensity env)),("sunShadows",Boolean (sunShadows env))]) ]
  where
    w=world p; c=camera p; env=environment p;settings=physicsSettings p
    int=Number . fromIntegral; num=Number . realToFrac; vec=Array . map num . vlist
    entity (e,n) = Object $ [("id",int e),("name",String n),("transform",trans (transforms w IM.! e))]
      ++ optional "mesh" (\m -> Object [("geometry",String (geometry m)),("texture",String (texture m))]) (IM.lookup e (meshes w))
      ++ optional "material" mat (IM.lookup e (materials w))
      ++ optional "script" String (IM.lookup e (scripts w))
      ++ optional "authored" (Object . map (\(key,fields)->(key,Object [(k,fieldJSON value) | (k,value)<-M.toAscList fields])) . M.toAscList) (IM.lookup e (authored w))
      ++ optional "light" (\l -> Object [("color",vec (lightColor l)),("intensity",num (intensity l))]) (IM.lookup e (lights w))
      ++ optional "parent" (\p' -> Object [("entity",int (parentEntity p')),("local",trans (localTransform p'))]) (IM.lookup e (parents w))
      ++ optional "body" (\b -> Object [("shape",String (if bodyShape b==Box then "box" else "sphere")),("mass",num (mass b)),("velocity",vec (velocity b)),("restitution",num (restitution b)),("friction",num (friction b)),("kinematic",Boolean (kinematic b)),("trigger",Boolean (trigger b)),("linearDamping",num (linearDamping b)),("angularDamping",num (angularDamping b))]) (IM.lookup e (bodies w))
    optional k f = maybe [] (\v -> [(k,f v)])
    trans t = Object [("position",vec (position t)),("rotation",vec (rotation t)),("scale",vec (scale t))]
    mat m = Object [("albedo",vec (albedo m)),("metallic",num (metallic m)),("roughness",num (roughness m)),("emission",num (emission m))]
    fieldJSON (FloatValue value)=num value
    fieldJSON (TextValue value)=String value
    fieldJSON (BoolValue value)=Boolean value
    fieldJSON (EntityValue ref)=Object [("entity",maybe Null int ref)]

decodeProject :: String -> Either String Project
decodeProject input = do
  j <- parseJSON input
  version <- field "schemaVersion" j >>= integer
  unless (version `elem` [1,2]) (Left "Unsupported project schema version")
  n <- field "name" j >>= string
  next <- field "nextId" j >>= integer
  es <- field "entities" j >>= array
  w <- foldM decodeEntity (emptyWorld {nextId=next}) es
  validateWorld w
  c <- field "camera" j
  cam <- Camera <$> vfield "eye" c <*> vfield "target" c <*> floatField "fov" c
  en <- field "environment" j
  env <- Environment <$> vfield "sunDirection" en <*> vfield "sunColor" en <*> floatField "sunPower" en <*> floatField "ambient" en <*> floatField "exposure" en
    <*> optionalField "skyTop" (vfield "skyTop" en) (skyTop defaultEnvironment) en
    <*> optionalField "skyBottom" (vfield "skyBottom" en) (skyBottom defaultEnvironment) en
    <*> optionalField "fogDensity" (floatField "fogDensity" en) 0 en
    <*> optionalField "sunShadows" (field "sunShadows" en >>= boolean) False en
  settings<-optionalField "physics" (do
    ph<-field "physics" j
    PhysicsSettings <$> vfield "gravity" ph <*> floatField "supportNormal" ph <*> floatField "supportDistance" ph) defaultPhysicsSettings j
  gameKey<-optionalField "haskellGame" (field "haskellGame" j >>= \v->case v of Null->Right Nothing;_->Just <$> string v) Nothing j
  let p=Project n (resolveWorld w) cam env settings gameKey
  validateProject p
  pure p
  where
    floatField k j = field k j >>= number >>= finiteFloat
    boolean (Boolean b) = Right b
    boolean _ = Left "Expected boolean"
    optionalField k value fallback j = object j >>= \fs -> if k `elem` map fst fs then value else Right fallback
    finiteFloat x = let f=realToFrac x in if finite f then Right f else Left "Number exceeds Float range"
    vfield k j = field k j >>= array >>= \xs -> if length xs/=3 then Left ("Expected vec3: "++k) else fromListV <$> mapM (number >=> finiteFloat) xs
    (>=>) a b x = a x >>= b
    integer :: JSON -> Either String Int
    integer v = do
      n <- number v
      maybe (Left "Expected integer ID") Right (readMaybe (show (truncate n :: Integer))) >>= \i -> if fromIntegral i == n then Right i else Left "Expected integer ID"
    decodeEntity w j = do
      e <- field "id" j >>= integer
      when (IM.member e (names w)) (Left "Duplicate entity ID")
      n <- field "name" j >>= string
      t <- field "transform" j
      tr <- Transform <$> vfield "position" t <*> vfield "rotation" t <*> vfield "scale" t
      fields <- object j
      let optional k decode = traverse decode (lookup k fields)
      m <- optional "mesh" (\o -> Mesh <$> (field "geometry" o >>= string) <*> (field "texture" o >>= string))
      mat <- optional "material" (\o -> Material <$> vfield "albedo" o <*> floatField "metallic" o <*> floatField "roughness" o <*> floatField "emission" o)
      script <- optional "script" string
      gameData <- optional "authored" (\o->object o >>= mapM (\(key,value)->do
        fs<-object value >>= mapM (\(k,v)->do
          decoded<-case v of
            Number _->FloatValue <$> (number v >>= finiteFloat)
            String txt->Right (TextValue txt)
            Boolean flag->Right (BoolValue flag)
            Object _->do ref<-field "entity" v; EntityValue <$> (case ref of Null->Right Nothing;_->Just <$> integer ref)
            _->Left "Invalid authored field"
          pure (k,decoded))
        pure (key,M.fromList fs)) >>= pure . M.fromList)
      light <- optional "light" (\o -> Light <$> vfield "color" o <*> floatField "intensity" o)
      parent <- optional "parent" (\o -> do
        owner<-field "entity" o >>= integer
        tr'<-field "local" o
        Parent owner <$> (Transform <$> vfield "position" tr' <*> vfield "rotation" tr' <*> vfield "scale" tr'))
      body <- optional "body" (\o -> do
        shape<-field "shape" o >>= string
        sh<-case shape of "box"->Right Box; "sphere"->Right Sphere; _->Left "Unknown physics shape"
        Body sh <$> floatField "mass" o <*> vfield "velocity" o <*> floatField "restitution" o <*> floatField "friction" o <*> pure False <*> optionalField "kinematic" (field "kinematic" o >>= boolean) False o
          <*> optionalField "trigger" (field "trigger" o >>= boolean) False o
          <*> optionalField "linearDamping" (floatField "linearDamping" o) 0.04 o
          <*> optionalField "angularDamping" (floatField "angularDamping" o) 0.08 o)
      let put v table = maybe table (\x -> IM.insert e x table) v
      pure w {names=IM.insert e n (names w),transforms=IM.insert e tr (transforms w),meshes=put m (meshes w),materials=put mat (materials w),scripts=put script (scripts w),lights=put light (lights w),bodies=put body (bodies w),parents=put parent (parents w),authored=put gameData (authored w)}

validateProject :: Project -> Either String ()
validateProject p = do
  validateWorld (world p)
  let c=camera p; env=environment p; ph=physicsSettings p
  unless (validV (eye c) && validV (target c) && finite (fov c) && fov c>1 && fov c<179 && eye c/=target c) (Left "Invalid camera")
  unless (all validV [sunDirection env,sunColor env,skyTop env,skyBottom env] && all finite [sunPower env,ambient env,exposure env,fogDensity env] && sunPower env>=0 && ambient env>=0 && exposure env>0 && fogDensity env>=0 && sunDirection env/=V3 0 0 0) (Left "Invalid environment")
  unless (validV (gravity ph) && finite (supportNormal ph) && supportNormal ph>=0 && supportNormal ph<=1 && finite (supportDistance ph) && supportDistance ph>=0) (Left "Invalid physics settings")
  when (haskellGame p==Just "") (Left "Empty Haskell game identifier")
  mapM_ assetPath (assetReferences (world p))

assetPath :: FilePath -> Either String ()
assetPath p
  | null p || normalise p=="." || isAbsolute p || ".." `elem` splitDirectories p = Left ("Asset paths must stay relative to the project: "++p)
  | map toLower (takeWhile (/=pathSeparator) (normalise p)) `elem` ["dist","engine","lib","unholy-player",projectFile,"play.command"] = Left ("Asset path conflicts with exported player: "++p)
  | otherwise = Right ()

builtins :: [String]
builtins = ["cube","sphere","plane","cylinder","torus"]

assetReferences :: World -> [FilePath]
assetReferences w = nub (IM.elems (scripts w) ++ [geometry m | m<-IM.elems (meshes w), geometry m `notElem` builtins] ++ [texture m | m<-IM.elems (meshes w),not (null (texture m))])

loadProject :: FilePath -> IO (Either String Project)
loadProject path = ioEither $ do
  source <- readFile path
  case decodeProject source of
    Left e -> ioError (userError e)
    Right p -> pure p

saveProject :: FilePath -> Project -> IO (Either String ())
saveProject path p = ioEither $ do
  either (ioError . userError) pure (validateProject p)
  createDirectoryIfMissing True (takeDirectory path)
  let temp=path++".tmp"
  writeFile temp (encodeJSON (projectJSON p)++"\n")
  renameFile temp path

checkAssets :: FilePath -> Project -> IO [String]
checkAssets root p = checkAssetFiles root (assetReferences (world p))

checkAssetFiles :: FilePath -> [FilePath] -> IO [String]
checkAssetFiles root refs = do
  base<-canonicalizePath root
  fmap concat $ forM refs $ \ref -> case assetPath ref of
    Left err->pure [err]
    Right ()->do
      linked<-or <$> mapM (\path->do exists<-doesPathExist path; if exists then pathIsSymbolicLink path else pure False) (scanl (</>) base (splitDirectories (normalise ref)))
      exists<-doesFileExist (base </> ref)
      pure (["Symbolic links are unsupported in project assets: "++ref | linked]++["Missing asset: "++ref | not exists])

ioEither :: IO a -> IO (Either String a)
ioEither act = do
  r <- try act
  pure $ case r of Left e -> Left (show (e::IOException)); Right v -> Right v

slug :: String -> String
slug s = let r=map (\c -> if isAlphaNum c then toLower c else '-') s in if null r then "untitled" else take 70 r

listAssets :: FilePath -> IO [FilePath]
listAssets root = do
  exists <- doesDirectoryExist (root </> "assets")
  if exists then walk "assets" else pure []
  where
    walk rel = do
      files <- listDirectory (root </> rel)
      fmap concat $ mapM (\f -> do
        let p=rel </> f
        link<-pathIsSymbolicLink (root </> p)
        dir<-doesDirectoryExist (root </> p)
        if link || "." `isPrefixOf` f then pure [] else if dir then walk p else pure [p]) files

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  linkRoot<-pathIsSymbolicLink from
  when linkRoot (ioError (userError ("Symbolic links are unsupported in exported assets: "++from)))
  createDirectoryIfMissing True to
  files<-listDirectory from
  forM_ files $ \f -> do
    let a=from </> f; b=to </> f
    link<-pathIsSymbolicLink a
    when link (ioError (userError ("Symbolic links are unsupported in exported assets: "++a)))
    dir<-doesDirectoryExist a
    if dir then copyTree a b else copyFile a b

-- Build a desktop folder with the current compiled runtime, project and assets.
-- On macOS relocate non-system dylibs so Homebrew is not required by the player.
buildProject :: FilePath -> FilePath -> Project -> IO (Either String FilePath)
buildProject = buildProjectWithAssets []

-- Include the selected game's declared assets as well as scene references.
-- The complete assets tree is retained for Lua/prefab/importer dependencies.
buildProjectWithAssets :: [FilePath] -> FilePath -> FilePath -> Project -> IO (Either String FilePath)
buildProjectWithAssets extra root engineAssets p = ioEither $ do
  either (ioError . userError) pure (validateProject p)
  let refs=nub (assetReferences (world p)++extra)
  missing<-checkAssetFiles root refs
  unless (null missing) (ioError (userError (unlines missing)))
  let output=root </> "dist" </> slug (projectName p)
      staging=output++".tmp"
  exists<-doesDirectoryExist staging
  when exists (removePathForcibly staging)
  createDirectoryIfMissing True staging
  exe<-getExecutablePath
  copyFile exe (staging </> "unholy-player")
  permissions<-getPermissions exe
  setPermissions (staging </> "unholy-player") permissions
  assetDirExists<-doesDirectoryExist (root </> "assets")
  when assetDirExists (copyTree (root </> "assets") (staging </> "assets"))
  forM_ refs $ \ref -> do
    createDirectoryIfMissing True (takeDirectory (staging </> ref))
    copyFile (root </> ref) (staging </> ref)
  copyTree (engineAssets </> "shaders") (staging </> "engine" </> "shaders")
  forM_ ["pbr.vs","pbr.fs","depth.vs","depth.fs","screen.vs","screen.fs","line.vs","line.fs"] $ \shader -> do
    shaderExists<-doesFileExist (staging </> "engine" </> "shaders" </> shader)
    unless shaderExists (ioError (userError ("Missing engine shader: "++shader)))
  writeFile (staging </> projectFile) (encodeJSON (projectJSON p)++"\n")
  writeFile (staging </> "Play.command") "#!/bin/sh\ncd -- \"$(dirname -- \"$0\")\"\nexec ./unholy-player --play project.hj.json --engine-assets engine\n"
  setPermissions (staging </> "Play.command") (permissions {executable=True})
  when (os=="darwin") (bundleMacLibraries staging)
  staged<-loadProject (staging </> projectFile)
  either (ioError.userError) (either (ioError.userError) pure . validateProject) staged
  stagedMissing<-checkAssetFiles staging refs
  unless (null stagedMissing) (ioError (userError (unlines stagedMissing)))
  old<-doesDirectoryExist output
  let backup=output++".previous"
  backupExists<-doesPathExist backup
  when backupExists (ioError (userError ("Previous export backup still exists: "++backup)))
  when old (renameDirectory output backup)
  renameDirectory staging output `onException` when old (renameDirectory backup output)
  when old (removePathForcibly backup)
  pure output

bundleMacLibraries :: FilePath -> IO ()
bundleMacLibraries root = do
  createDirectoryIfMissing True (root </> "lib")
  libs<-relocate [] (root </> "unholy-player") True
  forM_ libs $ \l -> callProcess "codesign" ["--force","--sign","-",root </> "lib" </> takeFileName l]
  callProcess "codesign" ["--force","--sign","-",root </> "unholy-player"]
  where
    relocate seen binary mainBinary = do
      output<-readProcess "otool" ["-L",binary] ""
      let deps=nub [d | line<-drop 1 (lines output), d<-take 1 (words line), isAbsolute d, not ("/usr/lib/" `isPrefixOf` d || "/System/" `isPrefixOf` d), takeFileName d/=takeFileName binary]
      foldM (\visited dep -> do
        let dest=root </> "lib" </> takeFileName dep
            relative=if mainBinary then "@loader_path/lib/" else "@loader_path/"
        callProcess "install_name_tool" ["-change",dep,relative++takeFileName dep,binary]
        if dep `elem` visited then pure visited else do
          copyFile dep dest
          callProcess "install_name_tool" ["-id","@rpath/"++takeFileName dep,dest]
          relocate (dep:visited) dest False) seen deps
