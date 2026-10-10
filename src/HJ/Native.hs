{-# LANGUAGE ForeignFunctionInterface #-}
module HJ.Native where

import HJ.Types
import Foreign
import Foreign.C
import Control.Monad (unless)
import qualified HJ.Renderer as R
import qualified HJ.Renderer.GL as G
import qualified Graphics.GL as GL
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (IOException, try, finally)
import Control.Concurrent (isCurrentThreadBound, myThreadId, ThreadId)

foreign import ccall unsafe "hj_init" cInit :: CString -> CString -> IO CInt
foreign import ccall unsafe "hj_close" cClose :: IO ()
foreign import ccall unsafe "hj_should_close" cShouldClose :: IO CInt
foreign import ccall unsafe "hj_width" cWidth :: IO CInt
foreign import ccall unsafe "hj_height" cHeight :: IO CInt
foreign import ccall unsafe "hj_dt" cDt :: IO CFloat
foreign import ccall unsafe "hj_begin" begin :: IO ()
foreign import ccall unsafe "hj_end" end :: IO ()
foreign import ccall unsafe "hj_screenshot" cScreenshot :: CString -> IO ()
foreign import ccall unsafe "hj_replay" cReplay :: CString -> IO CInt
foreign import ccall unsafe "hj_key" cKey :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "hj_clear_focus" clearFocus :: IO ()
foreign import ccall unsafe "hj_ui_enabled" cUIEnabled :: CInt -> IO ()
foreign import ccall unsafe "hj_inputs" cInputs :: Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_error" cError :: IO CString
foreign import ccall unsafe "hj_clear_error" cClearError :: IO ()
foreign import ccall unsafe "hj_rect" cRect :: CFloat -> CFloat -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall unsafe "hj_text" cText :: CFloat -> CFloat -> CFloat -> CString -> CInt -> IO ()
foreign import ccall unsafe "hj_button" cButton :: CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> IO CInt
foreign import ccall unsafe "hj_slider" cSlider :: CFloat -> CFloat -> CFloat -> CString -> CFloat -> CFloat -> CFloat -> IO CFloat
foreign import ccall unsafe "hj_textbox" cTextbox :: CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> IO CInt
foreign import ccall unsafe "hj_wheel" cWheel :: CFloat -> CFloat -> CFloat -> CFloat -> IO CFloat
foreign import ccall unsafe "hj_clip" cClip :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hj_unclip" unclip :: IO ()
foreign import ccall unsafe "hj_audio_pause" cAudioPause :: CInt -> IO ()
-- Compatibility facade for the existing editor. The window owns one explicit
-- Haskell Renderer; every GPU operation runs on the bound context thread.
{-# NOINLINE rendererState #-}
rendererState :: IORef (Maybe (ThreadId,R.Renderer))
rendererState = unsafePerformIO (newIORef Nothing)
{-# NOINLINE startupError #-}
startupError :: IORef String
startupError = unsafePerformIO (newIORef "")

initWindow :: String -> FilePath -> IO Bool
initWindow title path = do
  bound<-isCurrentThreadBound
  unless bound (ioError (userError "Window/renderer initialization requires a bound OS thread"))
  valid<-withCString title $ \t -> withCString path $ \p -> (/=0) <$> cInit t p
  if not valid then pure False else do
    initialized<-try (R.initialize path)
    case initialized of
      Left err -> writeIORef startupError (show (err::IOException)) >> cClose >> pure False
      Right renderer -> do
        owner<-myThreadId
        writeIORef rendererState (Just (owner,renderer))
        pure True
withRenderer :: (R.Renderer -> IO a) -> IO a
withRenderer action = do
  current<-readIORef rendererState
  owner<-myThreadId
  case current of
    Just (thread,renderer) | thread==owner -> action renderer
    _ -> ioError (userError "Renderer used outside its owning window thread")
close :: IO ()
close = do
  current<-readIORef rendererState
  case current of
    Nothing -> pure ()
    Just _ -> withRenderer (\r -> R.close r `finally` (writeIORef rendererState Nothing >> cClose))
unloadAssets :: IO ()
unloadAssets = withRenderer R.unloadAssets
pauseAudio :: Bool -> IO ()
pauseAudio pause = cAudioPause (if pause then 1 else 0)
shouldClose :: IO Bool
shouldClose = (/=0) <$> cShouldClose
dimensions :: IO (Float,Float)
dimensions = (,) <$> (fromIntegral <$> cWidth) <*> (fromIntegral <$> cHeight)
frameTime :: IO Float
frameTime = realToFrac <$> cDt
screenshot :: FilePath -> IO ()
screenshot path = G.preservingState $ \_ -> do
  -- EndDrawing swaps the window buffers. Capture the displayed front buffer,
  -- rather than the previous frame left in the back buffer.
  previous<-G.getInt GL.GL_READ_BUFFER
  GL.glReadBuffer GL.GL_FRONT
  withCString path cScreenshot `finally` GL.glReadBuffer (fromIntegral previous)
loadReplay :: FilePath -> IO Bool
loadReplay path = withCString path (fmap (/=0) . cReplay)
key :: Int -> Bool -> IO Bool
key k pressed = (/=0) <$> cKey (fromIntegral k) (if pressed then 1 else 0)
uiEnabled :: Bool -> IO ()
uiEnabled enabled = cUIEnabled (if enabled then 1 else 0)
inputs :: IO [Float]
inputs = allocaArray 4 $ \p -> cInputs p >> map realToFrac <$> peekArray 4 p
nativeError :: IO String
nativeError = do
  platform<-cError >>= peekCString
  startup<-readIORef startupError
  current<-readIORef rendererState
  renderErrors<-maybe (pure []) (R.takeErrors . snd) current
  pure (unlines (filter (not . null) ([platform,startup]++renderErrors)))
clearError :: IO ()
clearError = cClearError >> writeIORef startupError ""
rect :: Float -> Float -> Float -> Float -> Int -> IO ()
rect x y w h s = cRect (f x) (f y) (f w) (f h) (fromIntegral s)
text :: Float -> Float -> Float -> String -> Int -> IO ()
text x y size s style = withCString s $ \p -> cText (f x) (f y) (f size) p (fromIntegral style)
button :: Float -> Float -> Float -> Float -> String -> Int -> IO Bool
button x y w h s style = withCString s $ \p -> (/=0) <$> cButton (f x) (f y) (f w) (f h) p (fromIntegral style)
slider :: Float -> Float -> Float -> String -> Float -> Float -> Float -> IO Float
slider x y w label value lo hi = withCString label $ \p -> realToFrac <$> cSlider (f x) (f y) (f w) p (f value) (f lo) (f hi)
textbox :: Int -> Float -> Float -> Float -> Float -> String -> IO String
textbox ident x y w h s = withCString s $ \source -> allocaBytes 2048 $ \buffer -> do
  len<-cstringLength source
  copyBytes buffer source (min 2046 len)
  pokeByteOff buffer (min 2046 len) (0::CChar)
  _<-cTextbox (fromIntegral ident) (f x) (f y) (f w) (f h) buffer 2048
  peekCString buffer
  where cstringLength p = go p 0; go p n = do c<-peek p; if c==0 then pure n else go (p `plusPtr` 1) (n+1)
wheel :: Float -> Float -> Float -> Float -> IO Float
wheel x y w h = realToFrac <$> cWheel (f x) (f y) (f w) (f h)
clip :: Float -> Float -> Float -> Float -> IO ()
clip x y w h = cClip (f x) (f y) (f w) (f h)
focus :: V3 -> IO ()
focus v = withRenderer (\r -> R.focus r v)
getCamera :: IO Camera
getCamera = withRenderer R.getCamera
cameraData :: Camera -> [Float]
cameraData c = vlist (eye c)++vlist (target c)++[fov c]
cameraFromData :: [Float] -> Camera
cameraFromData d = Camera (fromListV (take 3 d)) (fromListV (take 3 (drop 3 d))) (max 2 (min 178 (d!!6)))
withFloats :: [Float] -> (Ptr CFloat -> IO a) -> IO a
withFloats values = withArray (map realToFrac values)
f :: Float -> CFloat
f = realToFrac

drawScene :: FilePath -> Project -> (Float,Float,Float,Float) -> Bool -> Bool -> Maybe Entity -> IO (Maybe Entity)
drawScene root p view edit grid selected = do
  screen<-dimensions
  withRenderer (\r -> R.drawScene r root p view screen edit grid selected)
