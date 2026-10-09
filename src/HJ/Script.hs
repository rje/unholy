{-# LANGUAGE ForeignFunctionInterface #-}
module HJ.Script (Runtime, startRuntime, stopRuntime, stepRuntime) where

import HJ.Types
import HJ.Native (withFloats,cameraData,cameraFromData)
import HJ.Physics
import qualified Data.IntMap.Strict as IM
import Foreign
import Foreign.C
import System.FilePath ((</>))
import Control.Monad (foldM, forM_, when)

type VM = Ptr ()
data Runtime = Runtime { virtualMachines :: IM.IntMap VM, physics :: Physics }
foreign import ccall unsafe "hj_lua_load" cLoad :: CString -> CString -> IO VM
foreign import ccall unsafe "hj_audio_stop" cStopAudio :: IO ()
foreign import ccall unsafe "hj_lua_close" cClose :: VM -> IO ()
foreign import ccall unsafe "hj_lua_valid" cValid :: VM -> IO CInt
foreign import ccall unsafe "hj_lua_message" cMessage :: VM -> IO CString
foreign import ccall unsafe "hj_lua_hud" cHud :: VM -> IO CString
foreign import ccall unsafe "hj_lua_clear" cClear :: VM -> IO ()
foreign import ccall unsafe "hj_lua_world_begin" cWorld :: VM -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_lua_entity" cEntity :: VM -> CInt -> CString -> Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "hj_lua_call" cCall :: VM -> CInt -> CInt -> CFloat -> Ptr CFloat -> IO CInt
foreign import ccall unsafe "hj_lua_read_entity" cRead :: VM -> CInt -> Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "hj_lua_read_camera" cCamera :: VM -> Ptr CFloat -> IO ()

startRuntime :: FilePath -> Project -> IO (Runtime,Project,[String])
startRuntime root p = do
  (vms,project,logs)<-foldM load (IM.empty,p,[]) (IM.toAscList (scripts (world p)))
  phy<-createPhysics (world project)
  pure (Runtime vms phy,project,logs)
  where
    load (vms,current,logs) (e,path) = do
      vm<-withCString (root </> path) $ \file -> withCString root (cLoad file)
      if vm==nullPtr then pure (vms,current,logs++["Could not allocate Lua VM"]) else do
        valid<-cValid vm
        message<-cMessage vm >>= peekCString
        cClear vm
        if valid==0 then cClose vm >> pure (vms,current,logs++[path++": "++message]) else do
          (next,msg,_,ok)<-runScript vm e True 0 [0,0,0,0] current
          if ok then pure (IM.insert e vm vms,next,logs++[message | not (null message)]++msg)
                else cClose vm >> pure (vms,current,logs++msg)

stopRuntime :: Runtime -> IO ()
stopRuntime r = do mapM_ cClose (IM.elems (virtualMachines r));closePhysics (physics r);cStopAudio

stepRuntime :: Float -> [Float] -> Runtime -> Project -> IO (Runtime,Project,[String],String)
stepRuntime dt input r p = do
  (vms,scripted,logs,hud)<-foldM update (IM.empty,p,[],"") (IM.toAscList (virtualMachines r))
  nextWorld<-stepPhysics (physics r) dt (world scripted)
  pure (r {virtualMachines=vms},scripted {world=nextWorld},logs,hud)
  where
    update (vms,current,logs,hud) (e,vm) = do
      (next,msg,status,ok)<-runScript vm e False dt input current
      when (not ok) (cClose vm)
      pure (if ok then IM.insert e vm vms else vms,next,logs++msg,if null status then hud else status)

runScript :: VM -> Entity -> Bool -> Float -> [Float] -> Project -> IO (Project,[String],String,Bool)
runScript vm e starting dt input p = do
  withFloats (cameraData (camera p)) (cWorld vm)
  forM_ (IM.toAscList (names w)) $ \(id',name) ->
    withCString name $ \n -> withFloats (entityData id' w) $ \d -> cEntity vm (fromIntegral id') n d (hasBody id' w)
  result<-withFloats input (cCall vm (fromIntegral e) (if starting then 1 else 0) (realToFrac dt))
  message<-cMessage vm >>= peekCString
  hud<-cHud vm >>= peekCString
  cClear vm
  if result/=0 then pure (p,["Entity "++show e++": "++message],hud,False) else do
    next<-foldM readEntity w (IM.keys (names w))
    cam<-withFloats (cameraData (camera p)) $ \d -> cCamera vm d >> cameraFromData . map realToFrac <$> peekArray 7 d
    let validCam=if validV (eye cam) && validV (target cam) && eye cam/=target cam then cam else camera p
    pure (p {world=next,camera=validCam},[message | not (null message)],hud,True)
  where
    w=world p
    readEntity current id' = withFloats (entityData id' current) $ \d -> do
      cRead vm (fromIntegral id') d (hasBody id' current)
      xs<-map realToFrac <$> peekArray 19 d
      let oldT=IM.findWithDefault identity id' (transforms current)
          oldM=IM.findWithDefault defaultMaterial id' (materials current)
          (t,m)=updateComponentData (take 15 xs) oldT oldM
          bs=IM.adjust (\b -> b {velocity=fromListV (take 3 (drop 15 xs))}) id' (bodies current)
      pure current {transforms=IM.insert id' t (transforms current),materials=if IM.member id' (materials current) then IM.insert id' m (materials current) else materials current,bodies=bs}

hasBody :: Entity -> World -> CInt
hasBody e w = if IM.member e (bodies w) then 1 else 0
entityData :: Entity -> World -> [Float]
entityData e w = componentData (IM.findWithDefault identity e (transforms w)) (IM.findWithDefault defaultMaterial e (materials w)) ++ vlist (maybe (V3 0 0 0) velocity b) ++ [if maybe False grounded b then 1 else 0]
  where b=IM.lookup e (bodies w)
