module RuntimeSpec (luaAccess, audioPolicy) where

import Unholy.Types
import Unholy.Script
import qualified Unholy.Audio as A
import qualified Data.IntMap.Strict as IM
import Control.Monad (unless, replicateM_, forM_)
import Control.Exception (try, IOException, finally)
import Data.IORef
import Data.List (isInfixOf)
import Foreign.Ptr (intPtrToPtr, ptrToIntPtr)
import System.Directory (createDirectoryIfMissing)

assert :: String -> Bool -> IO ()
assert label okay = unless okay (ioError (userError label))
luaAccess :: IO ()
luaAccess = do
  createDirectoryIfMissing True "test-output"
  writeFile "test-output/access.lua" $ unlines
    ["local held, position, ticks = nil, nil, 0"
    ,"return {start=function(self,dt,input,scene)"
    ," held=scene:get('Target'); position=held.position"
    ," assert(scene.by_id[held.id]==held and scene.by_name.Target==held)"
    ," local count=0; for name,entity in pairs(scene.by_name) do assert(entity.name==name); count=count+1 end"
    ," assert(count==3); assert(scene:get('missing')==nil); self.note='private'"
    ," end, update=function(self,dt,input,scene)"
    ," ticks=ticks+1; assert(self.note=='private'); assert(input.jump==false and input.restart==false)"
    ," held.position.x=held.position.x+dt; position.y=position.y+dt"
    ," self.position={x=ticks,y=0,z=0}; held.material.metallic=8; held.material.roughness=-1"
    ," held.material.color={x=-1,y=2,z=0.5}; self.scale.x=-2; scene.camera.fov=250"
    ," scene.hud='persistent '..ticks; print('tick',ticks,true) end}"]
  let (_,w0)=spawnMesh "Runner" "sphere" emptyWorld
      (_,w1)=spawnMesh "Target" "cube" w0
      (_,w2)=spawn "Unrelated" w1
      p=(emptyProject "Lazy access") {world=w2 {scripts=IM.singleton 1 "access.lua"}}
  (r,initial,logs)<-startRuntime "test-output" p
  assert (show logs) (null logs)
  (r1,first,err1,_)<-stepRuntime 0.25 [] r initial
  let external=first {world=(world first) {transforms=IM.adjust (\t -> t {position=V3 10 20 30}) 2 (transforms (world first))}}
  (r2,next,err2,hud)<-stepRuntime 0.5 [] r1 external
  stopRuntime r2
  let w=world next; mat=materials w IM.! 2
  assert "cached entity/vector references see current Haskell ECS" (position (transforms w IM.! 2)==V3 10.5 20.5 30)
  assert "whole vectors, fields and clamped material/camera values" (position (transforms w IM.! 1)==V3 2 0 0 && metallic mat==1 && roughness mat==0.04 && albedo mat==V3 0 1 0.5 && fov (camera next)==178)
  assert "script metadata/logs/HUD persist" (hud=="persistent 2" && any (isInfixOf "tick 1 true") err1 && any (isInfixOf "tick 2 true") err2)
  assert "unrelated entity stays untouched and gains no material" (IM.lookup 3 (transforms w)==IM.lookup 3 (transforms w2) && not (IM.member 3 (materials w)))
  forM_ ["self.position.x=99; error('rollback')", "self.position.x=99; self.position.y=0/0", "scene:sound('../outside.wav')", "scene:sound('missing.wav')"] $ \body -> do
    writeFile "test-output/access.lua" ("return {update=function(self,dt,input,scene) "++body++" end}")
    (bad,started,_)<-startRuntime "test-output" p
    (disabled,after,errors,_)<-stepRuntime 0.1 [] bad started
    (disabled2,after2,again,_)<-stepRuntime 0.1 [] disabled after
    stopRuntime disabled2
    assert ("failed callback contained: "++body++show errors) (not (null errors) && null again && world after==world started && after2==after)
  -- Each scripted entity retains its own module locals and Haskell-owned state.
  writeFile "test-output/access.lua" "local n=0; return {update=function(self) n=n+1; self.position.x=n end}"
  (isolated,started,_)<-startRuntime "test-output" (p {world=(world p) {scripts=IM.fromList [(1,"access.lua"),(2,"access.lua")]}})
  (done,both,errors,_)<-stepRuntime 0.1 [] isolated started
  stopRuntime done
  assert "Lua module instances are isolated" (null errors && all ((==V3 1 0 0) . position) [transforms (world both) IM.! 1,transforms (world both) IM.! 2])

audioPolicy :: IO ()
audioPolicy = do
  counter<-newIORef 0;active<-newIORef IM.empty;events<-newIORef []
  let record event = modifyIORef' events (++[event])
      key = fromIntegral . ptrToIntPtr
      allocate tag = do
        n<-atomicModifyIORef' counter (\i -> (i+1,i+1));record tag
        modifyIORef' active (IM.insert n False);pure (intPtrToPtr (fromIntegral n))
      set tag playing sound = record (tag++show (key sound)) >> modifyIORef' active (IM.insert (key sound) playing)
      backend=A.Backend (\path -> allocate ("load "++path)) (\_ -> allocate "alias")
        (\sound alias -> record ("unload "++show (key sound)++" "++show alias))
        (set "play " True) (set "stop " False) (set "pause " False) (set "resume " True)
        (\sound -> IM.findWithDefault False (key sound) <$> readIORef active)
        (\_ volume -> record ("volume "++show volume)) (\_ pitch -> record ("pitch "++show pitch))
        (record . ("master "++) . show) (record "close")
  engine<-A.newEngine backend
  (do
    A.playOn engine "clip.wav" 2 9 False;A.playOn engine "clip.wav" 0.5 1 False
    s<-A.stats engine;assert "overlapping effects share a decoded clip" (s==(1,2,False,False))
    modifyIORef' active (IM.insert 2 False)
    A.playOn engine "clip.wav" 1 1 False
    s1<-A.stats engine;assert "completed voice reused" (s1==s)
    replicateM_ 32 (A.playOn engine "clip.wav" 1 1 False)
    A.playOn engine "clip.wav" 0.2 1 True
    replicateM_ 8 (A.playOn engine "clip.wav" 0.2 1 True)
    before<-readIORef events
    assert "bounded voices, cached decode and sustained loop" (length (filter (=="load clip.wav") before)==1 && length (filter (=="alias") before)==9 && length (filter (=="play 10") before)==1)
    assert "audio parameters clamped" ("volume 1.0" `elem` before && "pitch 3.0" `elem` before)
    A.pauseOn engine True;A.pauseOn engine True
    A.playOn engine "clip.wav" 0 1 True
    A.muteOn engine True;A.muteOn engine False
    A.pauseOn engine False
    after<-readIORef events
    assert "pause once, resume active effects, silent loop stays stopped" (length (filter (=="pause 2") after)==1 && "resume 2" `elem` after && not ("resume 10" `elem` after) && "master 0.0" `elem` after && "master 0.7" `elem` after)
    A.stopOn engine
    stopped<-A.stats engine;assert "stop clears project clips and pause state" (stopped==(0,0,False,False))
    A.playOn engine "next-project.wav" 1 1 False
    ) `finally` A.closeEngine engine
  A.closeEngine engine
  events'<-readIORef events
  assert "device and voices released exactly once" (length (filter (=="close") events')==1 && length (filter (isInfixOf "unload ") events')==12)
  bad<-try (A.playOn engine "closed.wav" 1 1 False) :: IO (Either IOException ())
  assert "closed engine rejects playback" (either (const True) (const False) bad)
