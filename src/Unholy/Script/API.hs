{-# LANGUAGE OverloadedStrings #-}
module Unholy.Script.API (Context, newContext, beginCall, result, install, pushEntity, bindPhysics, bindAudio, pushEvent, commitPhysics) where

import Unholy.Types
import Unholy.Transform
import Unholy.Prefab
import Unholy.Physics
import Unholy.Project (assetPath)
import qualified Unholy.Audio as Audio
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
  , entityCache :: IORef Lua.Reference, namedEntities :: IORef (IM.IntMap String,M.Map String Entity), root :: FilePath
  , physicsRef :: IORef (Maybe Physics), prefabCache :: IORef (M.Map FilePath Prefab), pendingTeleports :: IORef [Entity], audioEngine :: IORef (Maybe Audio.Engine) }
newContext :: FilePath -> Project -> IO Context
newContext path p = Context <$> newIORef p <*> newIORef [] <*> newIORef "" <*> newIORef (Lua.toReference (fromIntegral Lua.noref))
  <*> newIORef (names (world p),nameIndex p) <*> pure path <*> newIORef Nothing <*> newIORef M.empty <*> newIORef [] <*> newIORef Nothing
nameIndex :: Project -> M.Map String Entity
nameIndex p=M.fromList [(name,e) | (e,name)<-IM.toAscList (names (world p))]
bindPhysics :: Context -> Physics -> IO ()
bindPhysics ctx phy=writeIORef (physicsRef ctx) (Just phy)
bindAudio :: Context -> Maybe Audio.Engine -> IO ()
bindAudio ctx engine=writeIORef (audioEngine ctx) engine
refreshNames :: Context -> Project -> IO ()
refreshNames ctx p = do
  (old,_)<-readIORef (namedEntities ctx)
  when (old/=names (world p)) (writeIORef (namedEntities ctx) (names (world p),nameIndex p))
beginCall :: Context -> Project -> IO ()
beginCall ctx p = writeIORef (projectRef ctx) p >> refreshNames ctx p >> writeIORef (messages ctx) [] >> writeIORef (hudRef ctx) "" >> writeIORef (pendingTeleports ctx) []
commitPhysics :: Context -> IO ()
commitPhysics ctx=do
  pending<-readIORef (pendingTeleports ctx)
  phy<-readIORef (physicsRef ctx)
  forM_ phy (\p->mapM_ (requestTeleport p) pending)
result :: Context -> IO (Project,[String],String)
result ctx = (,,) <$> readIORef (projectRef ctx) <*> readIORef (messages ctx) <*> readIORef (hudRef ctx)
current :: Context -> Lua.Lua Project
current = Lua.liftIO . readIORef . projectRef
modify :: Context -> (Project -> Project) -> Lua.Lua ()
modify ctx action = Lua.liftIO $ do
  modifyIORef' (projectRef ctx) action
  readIORef (projectRef ctx) >>= refreshNames ctx
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
  Lua.newtable;Lua.newtable;string "v";Lua.setfield (-2) "__mode";Lua.setmetatable (-2)
  cache<-Lua.ref Lua.registryindex;Lua.liftIO (writeIORef (entityCache ctx) cache)
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
      "spawn" -> spawnPrefab ctx
      "destroy" -> Lua.pushHaskellFunction (lookupEntity ctx 2 >>= mapM_ (\e -> modify ctx (\p -> p {world=destroy e (world p)})) >> pure 0)
      "raycast" -> raycast ctx
      "teleport" -> teleport ctx
      "physics" -> physicsProxy ctx
      "hud" -> Lua.liftIO (readIORef (hudRef ctx)) >>= string
      "sound" -> sound ctx False
      "loop" -> sound ctx True
      _ -> Lua.pushnil
    sceneSet "hud" i = readString i >>= Lua.liftIO . writeIORef (hudRef ctx)
    sceneSet key i | key `elem` ["get","by_id","by_name","camera","sound","loop","spawn","destroy","raycast","teleport","physics"] = readonly key i
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
    (_,index')<-Lua.liftIO (readIORef (namedEntities ctx))
    pure (M.lookup name index' >>= \e -> if IM.member e (names (world p)) then Just e else Nothing)
  else if kind==Lua.TypeTable then do
    void (Lua.getfield i "id");found<-lookupEntity ctx Lua.top;Lua.pop 1;pure found
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
    get key = do
      alive<-IM.member e . names . world <$> current ctx
      when (not alive && key `notElem` ["alive","id"]) (Lua.failLua ("Entity "++show e++" has been destroyed"))
      case key of
       "alive" -> Lua.pushboolean alive
       "id" -> Lua.pushinteger (fromIntegral e)
       "name" -> current ctx >>= string . IM.findWithDefault "" e . names . world
       "position" -> transformVector position (\v t -> t {position=v})
       "rotation" -> transformVector rotation (\v t -> t {rotation=v})
       "scale" -> transformVector scale (\v t -> t {scale=positive v})
       "material" -> materialProxy ctx e
       "body" -> do p<-current ctx;if IM.member e (bodies (world p)) then bodyProxy ctx e else Lua.pushnil
       _ -> Lua.pushnil
    set key i = do
      alive<-IM.member e . names . world <$> current ctx
      when (not alive) (Lua.failLua ("Entity "++show e++" has been destroyed"))
      case key of
       "position" -> readVector i >>= changeTransform (\v t -> t {position=v})
       "rotation" -> readVector i >>= changeTransform (\v t -> t {rotation=v})
       "scale" -> readVector i >>= changeTransform (\v t -> t {scale=positive v})
       "id" -> readonly key i
       "name" -> readonly key i
       "body" -> readonly key i
       "material" -> readonly key i
       _ -> custom key i
    transformVector field update = vector (current ctx >>= pure . field . IM.findWithDefault identity e . transforms . world) (changeTransform update)
    changeTransform update v = modify ctx $ \p -> p {world=setTransform e (update v (IM.findWithDefault identity e (transforms (world p)))) (world p)}
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
    get "trigger" = body >>= Lua.pushboolean . trigger
    get "mass" = body >>= number . mass
    get "grounded" = body >>= Lua.pushboolean . grounded
    get _ = Lua.pushnil
    set "velocity" i = readVector i >>= change
    set "trigger" i = readonly "trigger" i
    set "mass" i = readonly "mass" i
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
  engine<-Lua.liftIO (readIORef (audioEngine ctx))
  played<-Lua.liftIO (try (Audio.play engine (root ctx </> path) volume pitch looping) :: IO (Either IOException ()))
  either (Lua.failLua . displayException) pure played
  pure 0
  where optional i fallback = Lua.isnoneornil i >>= \empty -> if empty then pure fallback else readNumber i

withPhysics :: Context -> (Physics -> Lua.Lua a) -> Lua.Lua a
withPhysics ctx action = Lua.liftIO (readIORef (physicsRef ctx)) >>= maybe (Lua.failLua "Physics is unavailable") action
physicsProxy :: Context -> Lua.Lua ()
physicsProxy ctx = proxy get set
  where
    config=physicsSettings <$> current ctx
    change update=modify ctx (\p -> p {physicsSettings=update (physicsSettings p)})
    get "gravity"=vector (gravity <$> config) (\v -> change (\s -> s {gravity=v}))
    get _=Lua.pushnil
    set "gravity" i=readVector i >>= \v -> change (\s -> s {gravity=v})
    set key i=readonly key i
spawnPrefab :: Context -> Lua.Lua ()
spawnPrefab ctx = Lua.pushHaskellFunction $ do
  path<-readString 2
  either Lua.failLua pure (assetPath path)
  cached<-Lua.liftIO (M.lookup path <$> readIORef (prefabCache ctx))
  prefab<-case cached of
    Just p -> pure p
    Nothing -> do
      loaded<-Lua.liftIO (loadPrefab (root ctx) path)
      p<-either Lua.failLua pure loaded
      Lua.liftIO (modifyIORef' (prefabCache ctx) (M.insert path p));pure p
  empty<-Lua.isnoneornil 3
  placement<-if empty then pure identity else do
    void (Lua.getfield 3 "position");hasPosition<-not <$> Lua.isnil Lua.top
    pos<-if hasPosition then readVector Lua.top else readVector 3
    Lua.pop 1
    rot<-optionalVector 3 "rotation" (V3 0 0 0);size<-optionalVector 3 "scale" (V3 1 1 1)
    pure (Transform pos rot size)
  p<-current ctx
  (e,_,w)<-either Lua.failLua pure (instantiate prefab placement (world p))
  modify ctx (\project -> project {world=w})
  pushEntity ctx e;pure 1
optionalVector :: Lua.StackIndex -> Lua.Name -> V3 -> Lua.Lua V3
optionalVector i key fallback = do
  void (Lua.getfield i key);empty<-Lua.isnil Lua.top
  v<-if empty then pure fallback else readVector Lua.top
  Lua.pop 1;pure v
teleport :: Context -> Lua.Lua ()
teleport ctx = Lua.pushHaskellFunction $ do
  e<-lookupEntity ctx 2 >>= maybe (Lua.failLua "Teleport needs a live entity") pure
  pos<-readVector 3
  modify ctx (\p -> p {world=setTransform e ((transforms (world p) IM.! e) {position=pos}) (world p)})
  modify ctx (\p -> p {world=(world p) {bodies=IM.adjust (\b -> b {velocity=V3 0 0 0,grounded=False}) e (bodies (world p))}})
  Lua.liftIO (modifyIORef' (pendingTeleports ctx) (e:))
  pure 0
raycast :: Context -> Lua.Lua ()
raycast ctx = Lua.pushHaskellFunction $ do
  from<-readVector 2;to<-readVector 3
  ignore<-lookupEntity ctx 4
  p<-current ctx
  hit<-withPhysics ctx (\phy -> Lua.liftIO (raycastPhysics phy (world p) from to ignore))
  case hit of
    Nothing -> Lua.pushnil
    Just h -> do
      Lua.newtable;pushEntity ctx (hitEntity h);Lua.setfield (-2) "entity"
      plainVector (hitPoint h);Lua.setfield (-2) "point"
      plainVector (hitNormal h);Lua.setfield (-2) "normal"
      number (hitFraction h);Lua.setfield (-2) "fraction"
  pure 1
plainVector :: V3 -> Lua.Lua ()
plainVector v = do
  Lua.newtable
  forM_ (zip ["x","y","z"] (vlist v)) $ \(key,value) -> number value >> Lua.setfield (-2) key
pushEvent :: Context -> Entity -> CollisionEvent -> Lua.Lua ()
pushEvent ctx own event = do
  let c=collisionContact event;other=if contactA c==own then contactB c else contactA c
      normal=if contactA c==own then contactNormal c else fromListV (map negate (vlist (contactNormal c)))
  Lua.newtable
  string (collisionKind event);Lua.setfield (-2) "kind"
  Lua.pushinteger (fromIntegral other);Lua.setfield (-2) "other_id"
  p<-current ctx
  if IM.member other (names (world p)) then pushEntity ctx other else Lua.pushnil
  Lua.setfield (-2) "other"
  plainVector normal;Lua.setfield (-2) "normal"
  plainVector (contactPoint c);Lua.setfield (-2) "point"
  number (contactImpulse c);Lua.setfield (-2) "impulse"
  Lua.pushboolean (collisionTrigger event);Lua.setfield (-2) "trigger"
