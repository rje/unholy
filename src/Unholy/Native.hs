{-# LANGUAGE ForeignFunctionInterface #-}
-- Explicit window resources and Haskell UI/input policy. raygui implements widgets.
module Unholy.Native where

import Unholy.Types
import qualified Unholy.Gizmo as Gizmo
import Unholy.Renderer.Camera (Mouse(..), inside)
import qualified Unholy.Input as I
import qualified Unholy.Theme as T
import qualified Unholy.Audio as Audio
import qualified Unholy.Renderer as R
import qualified Unholy.Renderer.GL as G
import qualified Graphics.GL as GL
import Foreign
import Foreign.C
import Control.Monad (unless, when, forM_)
import Data.IORef
import qualified Data.IntSet as S
import Control.Exception (finally, onException, bracket)
import Control.Concurrent (isCurrentThreadBound, myThreadId, ThreadId)
import System.Info (os)
import Text.Printf (printf)

data Window = Window { renderer :: R.Renderer, audio :: Maybe Audio.Engine, owner :: ThreadId
  , uiState :: IORef UIState, closed :: IORef Bool }
data UIState = UIState { focusedText :: Maybe (Int,String), enabled :: Bool
  , clipRect :: Maybe (Float,Float,Float,Float), frameInput :: I.FrameInput
  , replay :: Maybe Replay }
data Replay = Replay (Ptr ()) Int Int Int -- handle, count, index, frame

foreign import ccall unsafe "hj_init" cInit :: CString -> CString -> CInt -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "hj_close" cClose :: IO ()
foreign import ccall unsafe "hj_should_close" cShouldClose :: IO CInt
foreign import ccall unsafe "hj_width" cWidth :: IO CInt
foreign import ccall unsafe "hj_height" cHeight :: IO CInt
foreign import ccall unsafe "hj_dt" cDt :: IO CFloat
foreign import ccall unsafe "hj_input" cInput :: Ptr CInt -> Ptr CInt -> Ptr Float -> IO ()
foreign import ccall unsafe "hj_widget_input" cWidgetInput :: Ptr CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "hj_begin" cBegin :: Word32 -> IO ()
foreign import ccall unsafe "hj_end" cEnd :: IO ()
foreign import ccall unsafe "hj_screenshot" cScreenshot :: CString -> IO ()
foreign import ccall unsafe "hj_replay_load" cReplay :: CString -> IO (Ptr ())
foreign import ccall unsafe "hj_replay_free" cReplayFree :: Ptr () -> IO ()
foreign import ccall unsafe "hj_replay_count" cReplayCount :: Ptr () -> IO CUInt
foreign import ccall unsafe "hj_replay_frame" cReplayFrame :: Ptr () -> CUInt -> IO CUInt
foreign import ccall unsafe "hj_replay_event" cReplayEvent :: Ptr () -> CUInt -> IO ()
foreign import ccall unsafe "hj_error" cError :: IO CString
foreign import ccall unsafe "hj_clear_error" cClearError :: IO ()
foreign import ccall unsafe "hj_style" cStyle :: CInt -> CInt -> Word32 -> IO ()
foreign import ccall unsafe "hj_widget_enabled" cEnabled :: CInt -> IO ()
foreign import ccall unsafe "hj_rect" cRect :: CFloat -> CFloat -> CFloat -> CFloat -> Word32 -> IO ()
foreign import ccall unsafe "hj_text" cText :: CFloat -> CFloat -> CFloat -> CString -> Word32 -> IO ()
foreign import ccall unsafe "hj_measure" cMeasure :: CString -> CFloat -> IO CFloat
foreign import ccall unsafe "hj_button" cButton :: CFloat -> CFloat -> CFloat -> CFloat -> CString -> IO CInt
foreign import ccall unsafe "hj_slider" cSlider :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO CFloat
foreign import ccall unsafe "hj_textbox" cTextbox :: CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "hj_clip" cClip :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hj_unclip" cUnclip :: IO ()

openWindow :: String -> FilePath -> IO Window
openWindow title path = do
  bound<-isCurrentThreadBound
  unless bound (ioError (userError "Window initialization requires a bound OS thread"))
  let fontPath=if os=="darwin" then "/System/Library/Fonts/Supplemental/Arial.ttf" else "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  valid<-withCString title $ \t -> withCString fontPath $ \font -> cInit t font 1440 900 1100 760 60 (4+64+8192+32)
  unless (valid/=0) (ioError (userError "Cannot create window"))
  forM_ T.styles $ \(control,property,value)->cStyle (fromIntegral control) (fromIntegral property) value
  sound<-Audio.openDevice `onException` cClose
  r<-R.initialize path `onException` (mapM_ Audio.closeEngine sound >> cClose)
  Window r sound <$> myThreadId <*> newIORef (UIState Nothing True Nothing I.emptyFrame Nothing) <*> newIORef False
withWindow :: String -> FilePath -> (Window -> IO a) -> IO a
withWindow title path=bracket (openWindow title path) close
checkWindow :: Window -> IO ()
checkWindow w=do
  thread<-myThreadId;dead<-readIORef (closed w)
  unless (thread==owner w && not dead) (ioError (userError "Window used outside its owning thread or after close"))
withRenderer :: Window -> (R.Renderer -> IO a) -> IO a
withRenderer w action=checkWindow w >> action (renderer w)
close :: Window -> IO ()
close w=do
  dead<-readIORef (closed w)
  unless dead $ do
    checkWindow w
    state<-readIORef (uiState w)
    R.close (renderer w) `finally` (forM_ (replay state) freeReplay >> mapM_ Audio.closeEngine (audio w) >> writeIORef (closed w) True >> cClose)
  where freeReplay (Replay p _ _ _)=cReplayFree p
unloadAssets :: Window -> IO ()
unloadAssets w=withRenderer w R.unloadAssets
pauseAudio :: Window -> Bool -> IO ()
pauseAudio w value=mapM_ (\e->Audio.pauseOn e value) (audio w)
shouldClose :: Window -> IO Bool
shouldClose w=checkWindow w >> ((/=0) <$> cShouldClose)
dimensions :: Window -> IO (Float,Float)
dimensions w=checkWindow w >> ((,) <$> (fromIntegral <$> cWidth) <*> (fromIntegral <$> cHeight))
frameTime :: Window -> IO Float
frameTime w=do
  checkWindow w
  state<-readIORef (uiState w)
  nextReplay<-traverse advanceReplay (replay state)
  snapshot<-allocaArray 512 $ \down -> allocaArray 512 $ \pressed -> allocaArray 13 $ \raw -> do
    cInput down pressed raw
    ds<-peekArray 512 down;ps<-peekArray 512 pressed;v<-peekArray 13 raw
    case v of
      [x,y,dx,dy,wheelValue,right,middle,left,held,released,platformPress,platformRelease,tap]->do
        let click=any (/=0) [left,platformPress,tap]
            release=any (/=0) [released,platformRelease]
        cWidgetInput pressed (b click) (b release)
        pure (I.FrameInput (S.fromList [i | (i,d)<-zip [0..] ds,d/=0]) (S.fromList [i | (i,p)<-zip [0..] ps,p/=0]) (Mouse x y dx dy wheelValue (right/=0) (middle/=0) click True) (held/=0) release)
      _->ioError (userError "Invalid native input snapshot")
  writeIORef (uiState w) state {frameInput=snapshot,replay=nextReplay}
  mute<-key w 77 True
  when mute (mapM_ Audio.toggleMute (audio w))
  realToFrac <$> cDt
  where
    advanceReplay (Replay p count index frame)=do
      let dispatch i | i>=count=pure i
                     | otherwise=do
                         due<-fromIntegral <$> cReplayFrame p (fromIntegral i)
                         if due>frame then pure i else cReplayEvent p (fromIntegral i) >> dispatch (i+1)
      next<-dispatch index
      pure (Replay p count next (frame+1))
begin, end :: Window -> IO ()
begin w=checkWindow w >> cBegin T.background
end w=checkWindow w >> cEnd
screenshot :: Window -> FilePath -> IO ()
screenshot w path=checkWindow w >> G.preservingState (\_ -> do
  previous<-G.getInt GL.GL_READ_BUFFER
  GL.glReadBuffer GL.GL_FRONT
  withCString path cScreenshot `finally` GL.glReadBuffer (fromIntegral previous))
loadReplay :: Window -> FilePath -> IO Bool
loadReplay w path=do
  checkWindow w
  p<-withCString path cReplay
  if p==nullPtr then pure False else do
    count<-fromIntegral <$> cReplayCount p
    if count==0 then cReplayFree p >> pure False else do
      state<-readIORef (uiState w)
      forM_ (replay state) (\(Replay previous _ _ _)->cReplayFree previous)
      writeIORef (uiState w) state {replay=Just (Replay p count 0 0)}
      pure True
key :: Window -> Int -> Bool -> IO Bool
key w k pressed=do
  state<-readIORef (uiState w)
  pure (focusedText state==Nothing && (if pressed then I.keyPressed else I.keyHeld) k (frameInput state))
clearFocus :: Window -> IO ()
clearFocus w=modifyIORef' (uiState w) (\s->s {focusedText=Nothing})
uiEnabled :: Window -> Bool -> IO ()
uiEnabled w value=modifyIORef' (uiState w) (\s->s {enabled=value})
input :: Window -> IO I.Input
input w=do s<-readIORef (uiState w);pure (I.gameInput (focusedText s/=Nothing) (frameInput s))
inputs :: Window -> IO [Float]
inputs w=I.inputValues <$> input w
nativeError :: Window -> IO String
nativeError w=do platform<-cError >>= peekCString;errors<-R.takeErrors (renderer w);pure (unlines (filter (not.null) (platform:errors)))
clearError :: Window -> IO ()
clearError _=cClearError
rect :: Window -> Float -> Float -> Float -> Float -> Int -> IO ()
rect window x y w h s=checkWindow window >> cRect (f x) (f y) (f w) (f h) (T.rectangleColor s)
text :: Window -> Float -> Float -> Float -> String -> Int -> IO ()
text window x y size s style=checkWindow window >> withCString s (\p->cText (f x) (f y) (f size) p (T.textColor style))
interaction :: UIState -> Bool
interaction s=enabled s && maybe True (\r->inside r (I.pointer (frameInput s))) (clipRect s)
widget :: Window -> IO UIState
widget w=do checkWindow w;s<-readIORef (uiState w);cEnabled (b (interaction s));pure s
button :: Window -> Float -> Float -> Float -> Float -> String -> Int -> IO Bool
button window x y w h label style=do
  _<-widget window
  cStyle 2 1 (T.buttonColor style);cStyle 2 0 (T.buttonBorder style)
  withCString label (\p->(/=0) <$> cButton (f x) (f y) (f w) (f h) p)
slider :: Window -> Float -> Float -> Float -> String -> Float -> Float -> Float -> IO Float
slider window x y w label value lo hi=do
  text window x y 14 label 1
  let val=printf "%.2f" value
  width<-withCString val (\p->realToFrac <$> cMeasure p 14)
  text window (x+w-width) y 14 val 0
  _<-widget window
  realToFrac <$> cSlider (f x) (f (y+21)) (f w) (f value) (f lo) (f hi)
textbox :: Window -> Int -> Float -> Float -> Float -> Float -> String -> IO String
textbox window ident x y w h source=do
  state<-widget window
  let clicked=enabled state && leftPressed (I.pointer (frameInput state))
      hit=interaction state && inside (x,y,w,h) (I.pointer (frameInput state))
      previous=focusedText state
      focusValue=if clicked then if hit then case previous of Just (i,s) | i==ident->Just (i,s);_->Just (ident,source)
                                    else case previous of Just (i,_) | i==ident->Nothing;_->previous
                 else previous
      editing=enabled state && maybe False ((==ident).fst) focusValue
      contents=if editing then maybe source snd focusValue else source
  (finished,value)<-withCString contents $ \p -> allocaBytes 2048 $ \buffer -> do
    bytes<-lengthArray0 0 p
    copyBytes buffer p (min 2047 bytes);pokeByteOff buffer (min 2047 bytes) (0::CChar)
    done<-cTextbox (f x) (f y) (f w) (f h) buffer 2048 (b editing)
    value<-peekCString buffer
    pure (done/=0,value)
  modifyIORef' (uiState window) (\s->s {focusedText=if editing then if finished then Nothing else Just (ident,value) else focusValue})
  pure value
wheel :: Window -> Float -> Float -> Float -> Float -> IO Float
wheel window x y w h=do
  s<-readIORef (uiState window)
  let mouse=I.pointer (frameInput s)
  pure (if interaction s && inside (x,y,w,h) mouse then mouseWheel mouse else 0)
clip :: Window -> Float -> Float -> Float -> Float -> IO ()
clip window x y w h=do modifyIORef' (uiState window) (\s->s {clipRect=Just (x,y,w,h)});cClip (f x) (f y) (f w) (f h)
unclip :: Window -> IO ()
unclip window=modifyIORef' (uiState window) (\s->s {clipRect=Nothing}) >> cUnclip
focus :: Window -> V3 -> IO ()
focus w v=withRenderer w (\r->R.focus r v)
getCamera :: Window -> IO Camera
getCamera w=withRenderer w R.getCamera
f :: Float -> CFloat
f=realToFrac
b :: Bool -> CInt
b value=if value then 1 else 0
drawScene :: Window -> FilePath -> Project -> (Float,Float,Float,Float) -> Bool -> Bool -> Maybe Entity -> IO (Maybe Entity)
drawScene window root p view edit grid selected=do
  screen<-dimensions window;s<-readIORef (uiState window)
  let raw=frameInput s;mouse=I.pointer raw
  withRenderer window (\r->R.drawScene r mouse {mouseEnabled=enabled s && focusedText s==Nothing} (I.leftDown raw) (I.keyHeld 340 raw) root p view screen edit grid selected)
setGizmoMode :: Window -> Maybe Gizmo.Mode -> IO ()
setGizmoMode w mode=withRenderer w (\r->R.setGizmoMode r mode)
takeGizmoEdit :: Window -> IO (Maybe Gizmo.Edit)
takeGizmoEdit w=withRenderer w R.takeGizmoEdit
getMouse :: Window -> IO Mouse
getMouse w=do s<-readIORef (uiState w);pure (I.pointer (frameInput s)) {mouseEnabled=enabled s && focusedText s==Nothing}
