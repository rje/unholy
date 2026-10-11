{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Unholy.Script (Runtime, startRuntime, startRuntimeWith, stopRuntime, stepRuntime) where

import Unholy.Types
import Unholy.Project (validateProject)
import Unholy.Physics
import qualified Unholy.Game as Game
import Unholy.Input (Input(..))
import qualified Unholy.Script.API as API
import qualified Unholy.Audio as Audio
import qualified HsLua.Core as Lua
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C (CInt(..))
import System.FilePath ((</>))
import Control.Monad (foldM, when, void)
import Control.Exception (try, onException, finally)

data VM = VM FilePath Lua.State Lua.Reference API.Context (IORef Bool)
data Runtime = Runtime
  { virtualMachines :: IM.IntMap VM, physics :: Physics
  , projectRoot :: FilePath, disabledScripts :: IM.IntMap FilePath
  , audioEngine :: Maybe Audio.Engine, runningGame :: Maybe Game.RunningGame, clock :: Float, ownedVMs :: IORef (IM.IntMap VM), runtimeClosed :: IORef Bool }
foreign import ccall unsafe "hj_lua_budget" cBudget :: Ptr () -> CInt -> IO ()
budget :: Lua.State -> Bool -> IO ()
budget (Lua.State ptr) enabled = cBudget (castPtr ptr) (if enabled then 1 else 0)
closeVM :: VM -> IO ()
closeVM (VM _ state _ _ closed) = do
  dead<-atomicModifyIORef' closed (\x->(True,x))
  when (not dead) (Lua.close state)
vmPath :: VM -> FilePath
vmPath (VM path _ _ _ _) = path

startRuntime :: FilePath -> Project -> IO (Runtime,Project,[String])
startRuntime = startRuntimeWith [] Nothing
startRuntimeWith :: [Game.SomeGame] -> Maybe Audio.Engine -> FilePath -> Project -> IO (Runtime,Project,[String])
startRuntimeWith registry sound root p = do
  either (ioError.userError) pure (validateProject p)
  definition<-either (ioError.userError) pure (Game.findGame registry p)
  missing<-Game.checkGameAssets registry root p
  when (not (null missing)) (ioError (userError (unlines missing)))
  phy<-createPhysics (world p)
  owned<-newIORef IM.empty
  closed<-newIORef False
  let ctx=Game.GameContext root phy sound
      cleanup=(readIORef owned >>= mapM_ closeVM) `finally` (closePhysics phy `finally` mapM_ Audio.stopOn sound)
      initialize=do
        (active,prepared,startLogs)<-case definition of
          Nothing->pure (Nothing,p,[])
          Just g->do
            (running,next,effects)<-Game.startGame ctx g p
            (logs,_)<-Game.applyEffects ctx effects
            pure (Just running,next,logs)
        configurePhysics phy (physicsSettings prepared)
        let runtime=Runtime IM.empty phy root IM.empty sound active 0 owned closed
        (r,next,logs,_)<-reconcile runtime prepared
        synchronizePhysics phy (world next)
        pure (r,next,startLogs++logs)
  initialize `onException` cleanup
stopRuntime :: Runtime -> IO ()
stopRuntime r = do
  closed<-atomicModifyIORef' (runtimeClosed r) (\x->(True,x))
  when (not closed) $
    (atomicModifyIORef' (ownedVMs r) (\xs->(IM.empty,xs)) >>= mapM_ closeVM) `finally`
      (closePhysics (physics r) `finally` mapM_ Audio.stopOn (audioEngine r))

-- Scripts may spawn other scripts from start. Bound that work per frame, while
-- failed scripts remain disabled until their component changes or is removed.
reconcile :: Runtime -> Project -> IO (Runtime,Project,[String],String)
reconcile initial p = loop (64::Int) initial p [] ""
  where
    loop remaining r current logs hud = do
      let assigned=scripts (world current)
          valid e vm=IM.lookup e assigned==Just (vmPath vm)
          (kept,removed)=IM.partitionWithKey valid (virtualMachines r)
          disabled=IM.filterWithKey (\e path -> IM.lookup e assigned==Just path) (disabledScripts r)
          pending=IM.toAscList (IM.difference (IM.difference assigned kept) disabled)
          pruned=r {virtualMachines=kept,disabledScripts=disabled}
      writeIORef (ownedVMs r) kept
      mapM_ closeVM (IM.elems removed)
      case pending of
        [] -> pure (pruned,current,logs,hud)
        _ | remaining==0 -> pure (pruned,current,logs++["Script start limit reached; remaining entities start next frame"],hud)
        (e,path):_ -> do
          (vm,next,msg,status)<-loadVM pruned e path current
          let updated=case vm of
                Just live -> pruned {virtualMachines=IM.insert e live kept}
                Nothing -> pruned {disabledScripts=IM.insert e path disabled}
          writeIORef (ownedVMs r) (virtualMachines updated)
          loop (remaining-1) updated next (logs++msg) (prefer hud status)
loadVM :: Runtime -> Entity -> FilePath -> Project -> IO (Maybe VM,Project,[String],String)
loadVM r e path p = do
  state<-Lua.newstate
  ctx<-API.newContext (projectRoot r) p
  API.bindPhysics ctx (physics r)
  API.bindAudio ctx (audioEngine r)
  loaded<-try $ Lua.runWith state $ do
    Lua.openlibs;API.install ctx
    Lua.liftIO (budget state True)
    status<-Lua.loadfile (Just (projectRoot r </> path))
    when (status/=Lua.OK) Lua.throwErrorAsException
    called<-Lua.pcall 0 1 Nothing
    Lua.liftIO (budget state False)
    when (called/=Lua.OK) Lua.throwErrorAsException
    table<-Lua.istable Lua.top
    when (not table) (Lua.failLua "A script must return a table with callbacks")
    Lua.ref Lua.registryindex
  budget state False
  case loaded of
    Left err -> Lua.close state >> pure (Nothing,p,[path++": "++Lua.exceptionMessage err],"")
    Right ref -> do
      (_,loadLogs,_)<-API.result ctx
      closed<-newIORef False
      let vm=VM path state ref ctx closed
      (next,msg,hud,ok)<-runScript vm e "start" Nothing 0 [0,0,0,0] p
      if ok then pure (Just vm,next,loadLogs++msg,hud)
            else closeVM vm >> pure (Nothing,p,loadLogs++msg,"")

stepRuntime :: Float -> [Float] -> Runtime -> Project -> IO (Runtime,Project,[String],String)
stepRuntime dt input r p = do
  closed<-readIORef (runtimeClosed r)
  when closed (ioError (userError "Runtime is closed"))
  when (not (finite dt) || dt<=0) (ioError (userError "Runtime step must be finite and positive"))
  (nativeReady,nativeProject,nativeLogs,nativeHud)<-runGame False [] r p
  (prepared,ready,firstLogs,firstHud)<-reconcile nativeReady nativeProject
  (updated,scripted,updateLogs,updateHud)<-foldM update (prepared,ready,[],"") (IM.toAscList (virtualMachines prepared))
  (spawned,beforePhysics,spawnLogs,spawnHud)<-reconcile updated scripted
  configurePhysics (physics r) (physicsSettings beforePhysics)
  nextWorld<-stepPhysics (physics r) dt (world beforePhysics)
  events<-collisionEvents (physics r)
  (nativeFinal,nativeAfter,afterLogs,afterHud)<-runGame True events spawned beforePhysics {world=nextWorld}
  (contacted,afterEvents,eventLogs,eventHud)<-foldM dispatch (nativeFinal,nativeAfter,[],"")
    [(e,event) | event<-events,e<-[contactA (collisionContact event),contactB (collisionContact event)]]
  (final,next,finalLogs,finalHud)<-reconcile contacted afterEvents
  synchronizePhysics (physics r) (world next)
  pure (final {clock=clock r+dt},next,concat [nativeLogs,firstLogs,updateLogs,spawnLogs,afterLogs,eventLogs,finalLogs],foldl prefer "" [nativeHud,firstHud,updateHud,spawnHud,afterHud,eventHud,finalHud])
  where
    runGame after events runtime current=case runningGame runtime of
      Nothing->pure (runtime,current,[],"")
      Just running->do
        let ctx=Game.GameContext (projectRoot runtime) (physics runtime) (audioEngine runtime)
            values=input++repeat 0
            controls=Input (values!!0) (values!!1) (values!!2>0.5) (values!!3>0.5) (values!!4>0.5)
        (updated,next,effects)<-Game.tickGame after events ctx (Game.Step dt (clock runtime+dt)) controls running current
        (logs,hud)<-Game.applyEffects ctx effects
        pure (runtime {runningGame=Just updated},next,logs,hud)
    update acc@(runtime,current,logs,hud) (e,vm)
      | IM.lookup e (scripts (world current))/=Just (vmPath vm) = pure acc
      | otherwise = invoke runtime current logs hud e vm "update" Nothing
    dispatch acc@(runtime,current,logs,hud) (e,event) = case IM.lookup e (virtualMachines runtime) of
      Just vm | IM.lookup e (scripts (world current))==Just (vmPath vm) ->
        invoke runtime current logs hud e vm (if collisionTrigger event then "on_trigger" else "on_collision") (Just event)
      _ -> pure acc
    invoke runtime current logs hud e vm callback event = do
      (next,msg,status,ok)<-runScript vm e callback event dt input current
      when (not ok) (closeVM vm)
      let updated=if ok then runtime else runtime
            {virtualMachines=IM.delete e (virtualMachines runtime),disabledScripts=IM.insert e (vmPath vm) (disabledScripts runtime)}
      writeIORef (ownedVMs runtime) (virtualMachines updated)
      pure (updated,next,logs++msg,prefer hud status)
prefer :: String -> String -> String
prefer old new=if null new then old else new

runScript :: VM -> Entity -> Lua.Name -> Maybe CollisionEvent -> Float -> [Float] -> Project -> IO (Project,[String],String,Bool)
runScript (VM _ state ref ctx _) e callback event dt input p = do
  API.beginCall ctx p
  called<-try $ Lua.runWith state $ do
    Lua.settop 0
    void (Lua.getref Lua.registryindex ref)
    void (Lua.getfield Lua.top callback)
    absent<-Lua.isnil Lua.top
    when (not absent) $ do
      function<-Lua.isfunction Lua.top
      when (not function) (Lua.failLua "Script callback must be a function")
      API.pushEntity ctx e
      nargs<-case event of
        Just contact -> API.pushEvent ctx e contact >> pure 3
        Nothing -> do
          Lua.pushnumber (realToFrac dt)
          Lua.newtable
          mapM_ (\(key,value) -> Lua.pushnumber (realToFrac value) >> Lua.setfield (-2) key)
            (zip ["x","z"] (take 2 (input++repeat 0)))
          mapM_ (\(key,value) -> Lua.pushboolean (value>0.5) >> Lua.setfield (-2) key)
            (zip ["jump","restart","spawn"] (take 3 (drop 2 (input++repeat 0))))
          pure 4
      void (Lua.getglobal "scene")
      Lua.liftIO (budget state True)
      status<-Lua.pcall nargs 0 Nothing
      Lua.liftIO (budget state False)
      when (status/=Lua.OK) Lua.throwErrorAsException
  budget state False
  Lua.runWith state (Lua.settop 0 :: Lua.Lua ())
  (next,logs,hud)<-API.result ctx
  case called of
    Left err -> pure (p,logs++["Entity "++show e++": "++Lua.exceptionMessage err],"",False)
    Right () -> do
      API.commitPhysics ctx
      let cam=camera next
          validCam=if validV (eye cam) && validV (target cam) && eye cam/=target cam then cam else camera p
      pure (next {camera=validCam},logs,hud,True)
