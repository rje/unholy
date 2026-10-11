module AuthoringSpec (authoredReferences,ownedResources) where

import Unholy
import Unholy.Project (decodeProject,projectJSON)
import Unholy.JSON (encodeJSON)
import Unholy.Prefab
import qualified Unholy.ECS as ECS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Control.Exception (try,SomeException,displayException)
import Data.List (isInfixOf)
import Control.Monad (unless)

assert :: String -> Bool -> IO ()
assert label good=unless good (ioError (userError label))

data Link = Link Float (Maybe Entity) deriving (Eq,Show)
link :: Component Link
link=Component "link" (Link 2 Nothing) [Field "speed" "Speed" (NumberField 0 10),Field "target" "Target" EntityField]
  (\(Link speed targetId)->M.fromList [("speed",FloatValue speed),("target",EntityValue targetId)])
  (\fs->Link <$> numberField "speed" fs <*> entityField "target" fs)

authoredReferences :: IO ()
authoredReferences=do
  let (root,w0)=spawn "Assembly" emptyWorld
      (child,w1)=spawn "Target" w0
      (external,w2)=spawn "Outside" w1
      w=setComponent link root (Link 3 (Just child)) w2 {parents=IM.singleton child (Parent root identity)}
      (copy,duplicated)=duplicate root w
  values<-either (ioError.userError) pure (readComponents link duplicated)
  assert "Duplicate remaps internal references" (ECS.lookup copy values==Just (Link 3 (Just (copy+1))))
  let outside=setComponent link root (Link 3 (Just external)) w
      (other,withExternal)=duplicate root outside
  externalValues<-either (ioError.userError) pure (readComponents link withExternal)
  assert "Duplicate preserves external references" (ECS.lookup other externalValues==Just (Link 3 (Just external)))
  assert "Prefab rejects external references" (either (const True) (const False) (makePrefab root outside))
  prefab<-either (ioError.userError) pure (makePrefab root w)
  (placed,_,instanced)<-either (ioError.userError) pure (instantiate prefab identity w)
  placedValues<-either (ioError.userError) pure (readComponents link instanced)
  assert "Prefab remaps references" (ECS.lookup placed placedValues==Just (Link 3 (Just (placed+1))))
  let removed=destroy child w
  removedValues<-either (ioError.userError) pure (readComponents link removed)
  assert "Deletion explicitly clears references" (ECS.lookup root removedValues==Just (Link 3 Nothing))
  let preserved=removed {authored=IM.adjust (M.insert "unavailable" (M.singleton "future" (TextValue "keep me"))) root (authored removed)}
      p=(emptyProject "Authored") {world=preserved}
  assert "Unknown authored data round-trips" (decodeProject (encodeJSON (projectJSON p))==Right p)
  assert "Duplicate component registration rejected" (validateComponents [SomeComponent link,SomeComponent link] w/=Right ())

ownedResources :: IO ()
ownedResources=do
  released<-newIORef ([]::[Int])
  let allocate ctx n=acquireGameResource ctx (pure n) (\v->modifyIORef' released (++[v]))
      definition=(simpleGame "resources" ()) {initialize= \ctx p->do
        _<-allocate ctx 1;_<-allocate ctx 2;pure ((),p,[])}
      project=(emptyProject "Resources") {haskellGame=Just "resources"}
  (r,p,_)<-startRuntimeWith [game definition] Nothing "test-output" project
  (r',_,_,_)<-stepGame (1/120) (Input 0 0 False False False) r p
  stopRuntime r
  stopRuntime r'
  order<-readIORef released
  assert "Game resources release LIFO and once across runtime handles" (order==[2,1])
  writeIORef released []
  let failing=definition {initialize= \ctx _->allocate ctx 3 >> ioError (userError "initialization failed")}
  failed<-try (startRuntimeWith [game failing] Nothing "test-output" project) :: IO (Either SomeException (Runtime,Project,[String]))
  afterFailure<-readIORef released
  assert "Initialization failure closes acquired game resources" (either (const True) (const False) failed && afterFailure==[3])
  let bothFail=definition {initialize= \ctx _->do
        _<-acquireGameResource ctx (pure ()) (\_->ioError (userError "release failed"))
        ioError (userError "original initialization error")}
  originalError<-try (startRuntimeWith [game bothFail] Nothing "test-output" project) :: IO (Either SomeException (Runtime,Project,[String]))
  assert "Cleanup failure preserves the original initialization error" (either (isInfixOf "original initialization error" . displayException) (const False) originalError)
  writeIORef released []
  let closeFailure=definition {initialize= \ctx p->do
        _<-allocate ctx 1
        _<-acquireGameResource ctx (pure ()) (\_->ioError (userError "release failed"))
        _<-allocate ctx 2
        pure ((),p,[])}
  (bad,_,_)<-startRuntimeWith [game closeFailure] Nothing "test-output" project
  result<-try (stopRuntime bad) :: IO (Either SomeException ())
  stopRuntime bad
  allReleased<-readIORef released
  assert "Cleanup attempts every resource despite a failing release" (either (const True) (const False) result && allReleased==[2,1])
