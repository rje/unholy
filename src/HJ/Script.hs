{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module HJ.Script (Runtime, startRuntime, stopRuntime, stepRuntime) where

import HJ.Types
import HJ.Physics
import qualified HJ.Script.API as API
import qualified HJ.Audio as Audio
import qualified HsLua.Core as Lua
import qualified Data.IntMap.Strict as IM
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C (CInt(..))
import System.FilePath ((</>))
import Control.Monad (foldM, when, void)
import Control.Exception (try, onException, finally)

-- Only the interpreter and its instruction hook are native. API closures access
-- the typed Haskell world directly; no full scene upload or readback is needed.
data VM = VM Lua.State Lua.Reference API.Context
data Runtime = Runtime { virtualMachines :: IM.IntMap VM, physics :: Physics }
foreign import ccall unsafe "hj_lua_budget" cBudget :: Ptr () -> CInt -> IO ()
budget :: Lua.State -> Bool -> IO ()
budget (Lua.State ptr) enabled = cBudget (castPtr ptr) (if enabled then 1 else 0)
closeVM :: VM -> IO ()
closeVM (VM state _ _) = Lua.close state

startRuntime :: FilePath -> Project -> IO (Runtime,Project,[String])
startRuntime root p = do
  (vms,project,logs)<-foldM load (IM.empty,p,[]) (IM.toAscList (scripts (world p)))
  phy<-createPhysics (world project) `onException` mapM_ closeVM (IM.elems vms)
  pure (Runtime vms phy,project,logs)
  where
    load (vms,current,logs) (e,path) = do
      state<-Lua.newstate
      ctx<-API.newContext root current
      loaded<-try $ Lua.runWith state $ do
        Lua.openlibs;API.install ctx
        Lua.liftIO (budget state True)
        status<-Lua.loadfile (Just (root </> path))
        when (status/=Lua.OK) Lua.throwErrorAsException
        called<-Lua.pcall 0 1 Nothing
        Lua.liftIO (budget state False)
        when (called/=Lua.OK) Lua.throwErrorAsException
        table<-Lua.istable Lua.top
        when (not table) (Lua.failLua "A script must return a table with start/update functions")
        Lua.ref Lua.registryindex
      case loaded of
        Left err -> Lua.close state >> pure (vms,current,logs++[path++": "++Lua.exceptionMessage err])
        Right ref -> do
          (_,loadLogs,_)<-API.result ctx
          let vm=VM state ref ctx
          (next,msg,_,ok)<-runScript vm e True 0 [0,0,0,0] current
          if ok then pure (IM.insert e vm vms,next,logs++loadLogs++msg)
                else closeVM vm >> pure (vms,current,logs++loadLogs++msg)

stopRuntime :: Runtime -> IO ()
stopRuntime r = mapM_ closeVM (IM.elems (virtualMachines r)) `finally` (closePhysics (physics r) `finally` Audio.stop)
stepRuntime :: Float -> [Float] -> Runtime -> Project -> IO (Runtime,Project,[String],String)
stepRuntime dt input r p = do
  (vms,scripted,logs,hud)<-foldM update (IM.empty,p,[],"") (IM.toAscList (virtualMachines r))
  nextWorld<-stepPhysics (physics r) dt (world scripted)
  pure (r {virtualMachines=vms},scripted {world=nextWorld},logs,hud)
  where
    update (vms,current,logs,hud) (e,vm) = do
      (next,msg,status,ok)<-runScript vm e False dt input current
      when (not ok) (closeVM vm)
      pure (if ok then IM.insert e vm vms else vms,next,logs++msg,if null status then hud else status)
runScript :: VM -> Entity -> Bool -> Float -> [Float] -> Project -> IO (Project,[String],String,Bool)
runScript (VM state ref ctx) e starting dt input p = do
  API.beginCall ctx p
  called<-try $ Lua.runWith state $ do
    Lua.settop 0
    void (Lua.getref Lua.registryindex ref)
    void (Lua.getfield Lua.top (if starting then "start" else "update"))
    absent<-Lua.isnil Lua.top
    when (not absent) $ do
      function<-Lua.isfunction Lua.top
      when (not function) (Lua.failLua "Script callback must be a function")
      API.pushEntity ctx e
      Lua.pushnumber (realToFrac dt)
      Lua.newtable
      mapM_ (\(key,value) -> Lua.pushnumber (realToFrac value) >> Lua.setfield (-2) key)
        (zip ["x","z"] (take 2 (input++repeat 0)))
      mapM_ (\(key,value) -> Lua.pushboolean (value>0.5) >> Lua.setfield (-2) key)
        (zip ["jump","restart"] (take 2 (drop 2 (input++repeat 0))))
      void (Lua.getglobal "scene")
      Lua.liftIO (budget state True)
      status<-Lua.pcall 4 0 Nothing
      Lua.liftIO (budget state False)
      when (status/=Lua.OK) Lua.throwErrorAsException
  budget state False
  Lua.runWith state (Lua.settop 0 :: Lua.Lua ())
  (next,logs,hud)<-API.result ctx
  case called of
    Left err -> pure (p,logs++["Entity "++show e++": "++Lua.exceptionMessage err],"",False)
    Right () -> do
      let cam=camera next
          validCam=if validV (eye cam) && validV (target cam) && eye cam/=target cam then cam else camera p
      pure (next {camera=validCam},logs,hud,True)
