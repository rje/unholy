{-# LANGUAGE OverloadedStrings #-}
module HJ.Script.API (Context, newContext, beginCall, result, install, pushEntity) where

import HJ.Types
import HJ.Project (assetPath)
import qualified HJ.Audio as Audio
import qualified HsLua.Core as Lua
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IORef
import Control.Monad (forM_, forM, void, when)
import System.FilePath ((</>))
import Control.Exception (try, IOException, displayException)

data Context = Context
  { projectRef :: IORef Project, messages :: IORef [String], hudRef :: IORef String
  , entityCache :: IORef Lua.Reference, namedEntities :: M.Map String Entity, root :: FilePath }
newContext :: FilePath -> Project -> IO Context
newContext path p = Context <$> newIORef p <*> newIORef [] <*> newIORef "" <*> newIORef (Lua.toReference (fromIntegral Lua.noref))
  <*> pure (M.fromList [(name,e) | (e,name)<-IM.toAscList (names (world p))]) <*> pure path
beginCall :: Context -> Project -> IO ()
beginCall ctx p = writeIORef (projectRef ctx) p >> writeIORef (messages ctx) [] >> writeIORef (hudRef ctx) ""
result :: Context -> IO (Project,[String],String)
result ctx = (,,) <$> readIORef (projectRef ctx) <*> readIORef (messages ctx) <*> readIORef (hudRef ctx)
current :: Context -> Lua.Lua Project
current = Lua.liftIO . readIORef . projectRef
modify :: Context -> (Project -> Project) -> Lua.Lua ()
modify ctx = Lua.liftIO . modifyIORef' (projectRef ctx)
string :: String -> Lua.Lua ()
string = Lua.pushstring . T.encodeUtf8 . T.pack
readString :: Lua.StackIndex -> Lua.Lua String
readString i = Lua.tostring i >>= maybe (Lua.failLua "Expected a string") (pure . T.unpack . T.decodeUtf8With (\_ _ -> Just '\xfffd'))
number :: Float -> Lua.Lua ()
number = Lua.pushnumber . realToFrac
readNumber :: Lua.StackIndex -> Lua.Lua Float
readNumber i = do
  n<-Lua.tonumber i
  case realToFrac <$> n of
    Just value | finite value -> pure value
    _ -> Lua.failLua "Expected a finite number"
readVector :: Lua.StackIndex -> Lua.Lua V3
readVector i = do
  absI<-Lua.absindex i
  components<-forM ["x","y","z"] $ \key -> do
    void (Lua.getfield absI key); value<-readNumber Lua.top;Lua.pop 1;pure value
  pure (fromListV components)
-- Empty proxy tables route engine fields to Haskell. Script-owned fields remain
-- ordinary Lua fields. Protected HsLua closures translate exceptions at the ABI.
proxy :: (String -> Lua.Lua ()) -> (String -> Lua.StackIndex -> Lua.Lua ()) -> Lua.Lua ()
proxy getter setter = do
  Lua.newtable;Lua.newtable
  Lua.pushHaskellFunction (readString 2 >>= getter >> pure 1);Lua.setfield (-2) "__index"
  Lua.pushHaskellFunction (readString 2 >>= \key -> setter key 3 >> pure 0);Lua.setfield (-2) "__newindex"
  Lua.pushboolean False;Lua.setfield (-2) "__metatable"
  Lua.setmetatable (-2)
custom :: String -> Lua.StackIndex -> Lua.Lua ()
custom key i = string key >> Lua.pushvalue i >> Lua.rawset 1
readonly :: String -> Lua.StackIndex -> Lua.Lua ()
readonly key _ = Lua.failLua (key++" is read-only")
vector :: Lua.Lua V3 -> (V3 -> Lua.Lua ()) -> Lua.Lua ()
vector getter setter = proxy get set
  where
    get key = do
      V3 x y z<-getter
      case key of "x"->number x;"y"->number y;"z"->number z;_->Lua.pushnil
    set key i = do
      v<-readNumber i;V3 x y z<-getter
      case key of
        "x"->setter (V3 v y z);"y"->setter (V3 x v z);"z"->setter (V3 x y v)
        _->readonly key i

install :: Context -> Lua.Lua ()
install ctx = do
  Lua.newtable;cache<-Lua.ref Lua.registryindex;Lua.liftIO (writeIORef (entityCache ctx) cache)
  Lua.pushHaskellFunction $ do
    Lua.StackIndex count<-Lua.gettop
    parts<-forM [1..count] $ \i -> do
      value<-Lua.tostring' (Lua.StackIndex i);Lua.pop 1;pure (T.unpack (T.decodeUtf8With (\_ _ -> Just '\xfffd') value))
    Lua.liftIO (modifyIORef' (messages ctx) (++[unwords parts]));pure 0
  Lua.setglobal "print"
  proxy sceneGet sceneSet;Lua.setglobal "scene"
  where
    sceneGet key = case key of
      "get" -> Lua.pushHaskellFunction (lookupEntity ctx 2 >>= maybe Lua.pushnil (pushEntity ctx) >> pure 1)
      "by_id" -> index ctx False
      "by_name" -> index ctx True
      "camera" -> cameraProxy ctx
      "hud" -> Lua.liftIO (readIORef (hudRef ctx)) >>= string
      "sound" -> sound ctx False
      "loop" -> sound ctx True
      _ -> Lua.pushnil
    sceneSet "hud" i = readString i >>= Lua.liftIO . writeIORef (hudRef ctx)
    sceneSet key i | key `elem` ["get","by_id","by_name","camera","sound","loop"] = readonly key i
                   | otherwise = custom key i
lookupEntity :: Context -> Lua.StackIndex -> Lua.Lua (Maybe Entity)
lookupEntity ctx i = do
  kind<-Lua.ltype i
  p<-current ctx
  if kind==Lua.TypeNumber then do
    n<-Lua.tointeger i
    let entity=fromIntegral <$> n
    pure (entity >>= \e -> if IM.member e (names (world p)) then Just e else Nothing)
  else if kind==Lua.TypeString then do
    name<-readString i
    pure (M.lookup name (namedEntities ctx) >>= \e -> if IM.member e (names (world p)) then Just e else Nothing)
  else pure Nothing
index :: Context -> Bool -> Lua.Lua ()
index ctx byName = do
  Lua.newtable;Lua.newtable
  Lua.pushHaskellFunction (lookupEntity ctx 2 >>= maybe Lua.pushnil (pushEntity ctx) >> pure 1)
  Lua.setfield (-2) "__index"
  Lua.pushHaskellFunction $ do
    void (Lua.getglobal "next");Lua.newtable
    p<-current ctx
    forM_ (IM.toAscList (names (world p))) $ \(e,name) -> do
      if byName then string name else Lua.pushinteger (fromIntegral e)
      pushEntity ctx e;Lua.rawset (-3)
    Lua.pushnil;pure 3
  Lua.setfield (-2) "__pairs"
  Lua.pushHaskellFunction (readonly "Entity index" 3 >> pure 0);Lua.setfield (-2) "__newindex"
  Lua.pushboolean False;Lua.setfield (-2) "__metatable";Lua.setmetatable (-2)
pushEntity :: Context -> Entity -> Lua.Lua ()
pushEntity ctx e = do
  cache<-Lua.liftIO (readIORef (entityCache ctx));void (Lua.getref Lua.registryindex cache)
  void (Lua.rawgeti (-1) (fromIntegral e));missing<-Lua.isnil Lua.top
  when missing $ do
    Lua.pop 1;proxy get set;Lua.pushvalue Lua.top;Lua.rawseti (-3) (fromIntegral e)
  Lua.remove (-2)
  where
    get key = case key of
      "id" -> Lua.pushinteger (fromIntegral e)
      "name" -> current ctx >>= string . IM.findWithDefault "" e . names . world
      "position" -> transformVector position (\v t -> t {position=v})
      "rotation" -> transformVector rotation (\v t -> t {rotation=v})
      "scale" -> transformVector scale (\v t -> t {scale=positive v})
      "material" -> materialProxy ctx e
      "body" -> do p<-current ctx;if IM.member e (bodies (world p)) then bodyProxy ctx e else Lua.pushnil
      _ -> Lua.pushnil
    set key i = case key of
      "position" -> readVector i >>= changeTransform (\v t -> t {position=v})
      "rotation" -> readVector i >>= changeTransform (\v t -> t {rotation=v})
      "scale" -> readVector i >>= changeTransform (\v t -> t {scale=positive v})
      "id" -> readonly key i
      "name" -> readonly key i
      "body" -> readonly key i
      "material" -> readonly key i
      _ -> custom key i
    transformVector field update = vector (current ctx >>= pure . field . IM.findWithDefault identity e . transforms . world) (changeTransform update)
    changeTransform update v = modify ctx $ \p -> p {world=(world p) {transforms=IM.adjust (update v) e (transforms (world p))}}
    positive = fromListV . map (max 0.001) . vlist
materialProxy :: Context -> Entity -> Lua.Lua ()
materialProxy ctx e = proxy get set
  where
    mat = current ctx >>= pure . IM.findWithDefault defaultMaterial e . materials . world
    change update = modify ctx $ \p -> p {world=(world p) {materials=IM.adjust update e (materials (world p))}}
    get key = case key of
      "color" -> vector (albedo <$> mat) (\v -> change (\m -> m {albedo=unitV v}))
      "metallic" -> mat >>= number . metallic
      "roughness" -> mat >>= number . roughness
      "emission" -> mat >>= number . emission
      _ -> Lua.pushnil
    set key i = case key of
      "color" -> readVector i >>= \v -> change (\m -> m {albedo=unitV v})
      "metallic" -> readNumber i >>= \v -> change (\m -> m {metallic=unit v})
      "roughness" -> readNumber i >>= \v -> change (\m -> m {roughness=max 0.04 (unit v)})
      "emission" -> readNumber i >>= \v -> change (\m -> m {emission=max 0 v})
      _ -> custom key i
    unit = max 0 . min 1
    unitV = fromListV . map unit . vlist
bodyProxy :: Context -> Entity -> Lua.Lua ()
bodyProxy ctx e = proxy get set
  where
    body = current ctx >>= pure . IM.findWithDefault defaultBody e . bodies . world
    change v = modify ctx $ \p -> p {world=(world p) {bodies=IM.adjust (\b -> b {velocity=v}) e (bodies (world p))}}
    get "velocity" = vector (velocity <$> body) change
    get "grounded" = body >>= Lua.pushboolean . grounded
    get _ = Lua.pushnil
    set "velocity" i = readVector i >>= change
    set "grounded" i = readonly "grounded" i
    set key i = custom key i
cameraProxy :: Context -> Lua.Lua ()
cameraProxy ctx = proxy get set
  where
    cam = camera <$> current ctx
    change update = modify ctx (\p -> p {camera=update (camera p)})
    get "eye" = vector (eye <$> cam) (\v -> change (\c -> c {eye=v}))
    get "target" = vector (target <$> cam) (\v -> change (\c -> c {target=v}))
    get "fov" = cam >>= number . fov
    get _ = Lua.pushnil
    set "eye" i = readVector i >>= \v -> change (\c -> c {eye=v})
    set "target" i = readVector i >>= \v -> change (\c -> c {target=v})
    set "fov" i = readNumber i >>= \v -> change (\c -> c {fov=max 2 (min 178 v)})
    set key i = custom key i
sound :: Context -> Bool -> Lua.Lua ()
sound ctx looping = Lua.pushHaskellFunction $ do
  path<-readString 2
  either Lua.failLua pure (assetPath path)
  volume<-optional 3 1;pitch<-optional 4 1
  played<-Lua.liftIO (try (Audio.play (root ctx </> path) volume pitch looping) :: IO (Either IOException ()))
  either (Lua.failLua . displayException) pure played
  pure 0
  where optional i fallback = Lua.isnoneornil i >>= \empty -> if empty then pure fallback else readNumber i
