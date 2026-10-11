{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Unholy.Script (Runtime, startRuntime, stopRuntime, stepRuntime) where

import Unholy.Types
import Unholy.Physics
import qualified Unholy.Script.API as API
import qualified Unholy.Audio as Audio
import qualified HsLua.Core as Lua
import qualified Data.IntMap.Strict as IM
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C (CInt(..))
import System.FilePath ((</>))
import Control.Monad (foldM, when, void)
import Control.Exception (try, onException, finally)

data VM = VM FilePath Lua.State Lua.Reference API.Context
data Runtime = Runtime
  { virtualMachines :: IM.IntMap VM, physics :: Physics
  , projectRoot :: FilePath, disabledScripts :: IM.IntMap FilePath }
foreign import ccall unsafe "hj_lua_budget" cBudget :: Ptr () -> CInt -> IO ()
budget :: Lua.State -> Bool -> IO ()
budget (Lua.State ptr) enabled = cBudget (castPtr ptr) (if enabled then 1 else 0)
closeVM :: VM -> IO ()
closeVM (VM _ state _ _) = Lua.close state
vmPath :: VM -> FilePath
vmPath (VM path _ _ _) = path

startRuntime :: FilePath -> Project -> IO (Runtime,Project,[String])
startRuntime root p = do
  phy<-createPhysics (world p)
  configurePhysics phy (physicsSettings p)
  let runtime=Runtime IM.empty phy root IM.empty
  (r,next,logs,_)<-reconcile runtime p `onException` closePhysics phy
  synchronizePhysics phy (world next)
  pure (r,next,logs)
stopRuntime :: Runtime -> IO ()
stopRuntime r = mapM_ closeVM (IM.elems (virtualMachines r)) `finally` (closePhysics (physics r) `finally` Audio.stop)

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
      mapM_ closeVM (IM.elems removed)
      case pending of
        [] -> pure (pruned,current,logs,hud)
        _ | remaining==0 -> pure (pruned,current,logs++["Script start limit reached; remaining entities start next frame"],hud)
        (e,path):_ -> do
          (vm,next,msg,status)<-loadVM pruned e path current
          let updated=case vm of
                Just live -> pruned {virtualMachines=IM.insert e live kept}
                Nothing -> pruned {disabledScripts=IM.insert e path disabled}
          loop (remaining-1) updated next (logs++msg) (prefer hud status)
loadVM :: Runtime -> Entity -> FilePath -> Project -> IO (Maybe VM,Project,[String],String)
loadVM r e path p = do
  state<-Lua.newstate
  ctx<-API.newContext (projectRoot r) p
  API.bindPhysics ctx (physics r)
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
      let vm=VM path state ref ctx
      (next,msg,hud,ok)<-runScript vm e "start" Nothing 0 [0,0,0,0] p
      if ok then pure (Just vm,next,loadLogs++msg,hud)
            else closeVM vm >> pure (Nothing,p,loadLogs++msg,"")

stepRuntime :: Float -> [Float] -> Runtime -> Project -> IO (Runtime,Project,[String],String)
stepRuntime dt input r p = do
  (prepared,ready,firstLogs,firstHud)<-reconcile r p
  (updated,scripted,updateLogs,updateHud)<-foldM update (prepared,ready,[],"") (IM.toAscList (virtualMachines prepared))
  (spawned,beforePhysics,spawnLogs,spawnHud)<-reconcile updated scripted
  configurePhysics (physics r) (physicsSettings beforePhysics)
  nextWorld<-stepPhysics (physics r) dt (world beforePhysics)
  events<-collisionEvents (physics r)
  (contacted,afterEvents,eventLogs,eventHud)<-foldM dispatch (spawned,beforePhysics {world=nextWorld},[],"")
    [(e,event) | event<-events,e<-[contactA (collisionContact event),contactB (collisionContact event)]]
  (final,next,finalLogs,finalHud)<-reconcile contacted afterEvents
  synchronizePhysics (physics r) (world next)
  pure (final,next,concat [firstLogs,updateLogs,spawnLogs,eventLogs,finalLogs],foldl prefer "" [firstHud,updateHud,spawnHud,eventHud,finalHud])
  where
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
      pure (updated,next,logs++msg,prefer hud status)
prefer :: String -> String -> String
prefer old new=if null new then old else new

runScript :: VM -> Entity -> Lua.Name -> Maybe CollisionEvent -> Float -> [Float] -> Project -> IO (Project,[String],String,Bool)
runScript (VM _ state ref ctx) e callback event dt input p = do
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
