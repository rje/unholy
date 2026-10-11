{-# LANGUAGE ForeignFunctionInterface #-}
module Unholy.Audio
  ( Backend(..), Engine, newEngine, closeEngine, playOn, pauseOn, muteOn, stopOn, stats
  , initialize, close, play, pause, toggleMute, stop, mixStats ) where

import Foreign
import Foreign.C
import Data.IORef
import qualified Data.Map.Strict as M
import Control.Monad (forM, forM_, when, unless)
import Control.Exception (finally)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Unholy.Types (finite)

type Sound = Ptr ()
-- Injectable device operations keep policy testable without speakers or a window.
data Backend = Backend
  { loadClip :: FilePath -> IO Sound, aliasClip :: Sound -> IO Sound
  , unloadClip :: Sound -> Bool -> IO (), playVoice :: Sound -> IO ()
  , stopVoice :: Sound -> IO (), pauseVoice :: Sound -> IO (), resumeVoice :: Sound -> IO ()
  , playingVoice :: Sound -> IO Bool, volumeVoice :: Sound -> Float -> IO ()
  , pitchVoice :: Sound -> Float -> IO (), masterVolume :: Float -> IO (), closeDevice :: IO () }
data Voice = Voice { handle :: Sound, resume :: Bool }
data Clip = Clip { source :: Sound, loopVoice :: Maybe Voice, shots :: [Voice] }
data State = State { clips :: M.Map FilePath Clip, paused :: Bool, muted :: Bool, disposed :: Bool }
data Engine = Engine Backend (IORef State)

newEngine :: Backend -> IO Engine
newEngine backend = do
  masterVolume backend 0.7
  Engine backend <$> newIORef (State M.empty False False False)
voices :: Clip -> [Voice]
voices clip = maybe [] (:[]) (loopVoice clip) ++ shots clip
stats :: Engine -> IO (Int,Int,Bool,Bool)
stats (Engine _ ref) = do
  s<-readIORef ref
  pure (M.size (clips s),sum (map (length . voices) (M.elems (clips s))),paused s,muted s)

playOn :: Engine -> FilePath -> Float -> Float -> Bool -> IO ()
playOn (Engine backend ref) path rawVolume rawPitch looping = do
  s<-readIORef ref
  when (disposed s) (ioError (userError "Audio engine is closed"))
  let volume=bounded 0 1 rawVolume; pitch=bounded 0.25 3 rawPitch
  clip<-case M.lookup path (clips s) of
    Just cached -> pure cached
    Nothing -> do
      when (M.size (clips s)>=64) (ioError (userError "Audio clip limit (64) reached"))
      sample<-loadClip backend path
      when (sample==nullPtr) (ioError (userError ("Cannot decode audio: "++path)))
      pure (Clip sample Nothing [])
  let fresh = do
        sound<-aliasClip backend (source clip)
        when (sound==nullPtr) (ioError (userError "Cannot allocate audio voice"))
        pure (Voice sound False)
  -- Publish decoded ownership before allocating aliases so failures still clean up.
  writeIORef ref s {clips=M.insert path clip (clips s)}
  (voice,rest)<-if looping then do
      v<-maybe fresh pure (loopVoice clip)
      pure (v,shots clip)
    else do
      available<-forM (shots clip) $ \v -> do
        active<-playingVoice backend (handle v)
        pure (v,active || resume v)
      case break (not . snd) available of
        (before,(v,_):after) -> pure (v,map fst (before++after))
        _ | length (shots clip)<8 -> do v<-fresh;pure (v,shots clip)
          | otherwise -> pure (last (shots clip),init (shots clip))
  volumeVoice backend (handle voice) volume
  pitchVoice backend (handle voice) pitch
  active<-playingVoice backend (handle voice)
  nextVoice<-if volume<=0 then stopVoice backend (handle voice) >> pure voice {resume=False}
    else if looping && (active || resume voice) then pure voice
    else do
      playVoice backend (handle voice)
      when (paused s) (pauseVoice backend (handle voice))
      pure voice {resume=paused s}
  let nextClip=if looping then clip {loopVoice=Just nextVoice} else clip {shots=nextVoice:rest}
  modifyIORef' ref (\current -> current {clips=M.insert path nextClip (clips current)})
  where bounded lo hi x = if finite x then max lo (min hi x) else lo

pauseOn :: Engine -> Bool -> IO ()
pauseOn (Engine backend ref) value = do
  s<-readIORef ref
  unless (disposed s || value==paused s) $ do
    let update v = if value then do
          active<-playingVoice backend (handle v)
          when active (pauseVoice backend (handle v))
          pure v {resume=active}
        else do
          when (resume v) (resumeVoice backend (handle v))
          pure v {resume=False}
    updated<-forM (clips s) $ \clip -> do
      loop<-traverse update (loopVoice clip); oneShots<-mapM update (shots clip)
      pure clip {loopVoice=loop,shots=oneShots}
    writeIORef ref s {clips=updated,paused=value}
muteOn :: Engine -> Bool -> IO ()
muteOn (Engine backend ref) value = do
  s<-readIORef ref
  unless (disposed s) $ masterVolume backend (if value then 0 else 0.7) >> writeIORef ref s {muted=value}
stopOn :: Engine -> IO ()
stopOn (Engine backend ref) = do
  s<-readIORef ref
  -- Aliases must be released before their shared sample data.
  forM_ (M.elems (clips s)) $ \clip -> do
    forM_ (voices clip) $ \v -> stopVoice backend (handle v) >> unloadClip backend (handle v) True
    unloadClip backend (source clip) False
  writeIORef ref s {clips=M.empty,paused=False}
closeEngine :: Engine -> IO ()
closeEngine engine@(Engine backend ref) = do
  s<-readIORef ref
  unless (disposed s) $ stopOn engine `finally` (modifyIORef' ref (\x -> x {disposed=True}) >> closeDevice backend)

{-# NOINLINE currentEngine #-}
currentEngine :: IORef (Maybe Engine)
currentEngine = unsafePerformIO (newIORef Nothing)
initialize :: IO ()
initialize = do
  close
  ready<-cInit
  when (ready/=0) (newEngine nativeBackend >>= writeIORef currentEngine . Just)
close :: IO ()
close = do
  current<-atomicModifyIORef' currentEngine (\x -> (Nothing,x))
  mapM_ closeEngine current
play :: FilePath -> Float -> Float -> Bool -> IO ()
play path volume pitch looping = do
  exists<-doesFileExist path
  unless exists (ioError (userError ("Audio asset missing: "++path)))
  readIORef currentEngine >>= mapM_ (\engine -> playOn engine path volume pitch looping)
pause :: Bool -> IO ()
pause value = readIORef currentEngine >>= mapM_ (\engine -> pauseOn engine value)
toggleMute :: IO ()
toggleMute = readIORef currentEngine >>= mapM_ (\engine@(Engine _ ref) -> readIORef ref >>= muteOn engine . not . muted)
stop :: IO ()
stop = readIORef currentEngine >>= mapM_ stopOn
mixStats :: IO (Integer,Float)
mixStats = (,) <$> (fromIntegral <$> cFrames) <*> (realToFrac <$> cPeak)

foreign import ccall unsafe "hj_audio_init" cInit :: IO CInt
foreign import ccall unsafe "hj_audio_close" cClose :: IO ()
foreign import ccall unsafe "hj_audio_frames" cFrames :: IO CULong
foreign import ccall unsafe "hj_audio_peak" cPeak :: IO CFloat
foreign import ccall unsafe "hj_audio_load" cLoad :: CString -> IO Sound
foreign import ccall unsafe "hj_audio_alias" cAlias :: Sound -> IO Sound
foreign import ccall unsafe "hj_audio_unload" cUnload :: Sound -> CInt -> IO ()
foreign import ccall unsafe "hj_audio_play" cPlay :: Sound -> IO ()
foreign import ccall unsafe "hj_audio_stop" cStop :: Sound -> IO ()
foreign import ccall unsafe "hj_audio_pause" cPause :: Sound -> IO ()
foreign import ccall unsafe "hj_audio_resume" cResume :: Sound -> IO ()
foreign import ccall unsafe "hj_audio_playing" cPlaying :: Sound -> IO CInt
foreign import ccall unsafe "hj_audio_volume" cVolume :: Sound -> CFloat -> IO ()
foreign import ccall unsafe "hj_audio_pitch" cPitch :: Sound -> CFloat -> IO ()
foreign import ccall unsafe "hj_audio_master" cMaster :: CFloat -> IO ()
nativeBackend :: Backend
nativeBackend = Backend (\path -> withCString path cLoad) cAlias
  (\sound alias -> cUnload sound (if alias then 1 else 0)) cPlay cStop cPause cResume
  (fmap (/=0) . cPlaying) (\sound -> cVolume sound . realToFrac)
  (\sound -> cPitch sound . realToFrac) (cMaster . realToFrac) cClose
