{-# LANGUAGE ForeignFunctionInterface #-}
module HJ.Native where

import HJ.Types
import Foreign
import Foreign.C
import Control.Monad (forM_, when)
import System.FilePath ((</>))
import HJ.Project (builtins)
import qualified Data.IntMap.Strict

foreign import ccall unsafe "hj_init" cInit :: CString -> CString -> IO CInt
foreign import ccall unsafe "hj_close" close :: IO ()
foreign import ccall unsafe "hj_unload_assets" unloadAssets :: IO ()
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
foreign import ccall unsafe "hj_clear_error" clearError :: IO ()
foreign import ccall unsafe "hj_rect" cRect :: CFloat -> CFloat -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall unsafe "hj_text" cText :: CFloat -> CFloat -> CFloat -> CString -> CInt -> IO ()
foreign import ccall unsafe "hj_button" cButton :: CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> IO CInt
foreign import ccall unsafe "hj_slider" cSlider :: CFloat -> CFloat -> CFloat -> CString -> CFloat -> CFloat -> CFloat -> IO CFloat
foreign import ccall unsafe "hj_textbox" cTextbox :: CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CString -> CInt -> IO CInt
foreign import ccall unsafe "hj_wheel" cWheel :: CFloat -> CFloat -> CFloat -> CFloat -> IO CFloat
foreign import ccall unsafe "hj_clip" cClip :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hj_unclip" unclip :: IO ()
foreign import ccall unsafe "hj_focus" cFocus :: Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_get_camera" cGetCamera :: Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_scene_begin" cSceneBegin :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "hj_light" cLight :: CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_light_count" cLightCount :: CInt -> IO ()
foreign import ccall unsafe "hj_draw_entity" cDrawEntity :: CInt -> Ptr CFloat -> CString -> CString -> CInt -> IO ()
foreign import ccall unsafe "hj_draw_light" cDrawLight :: Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "hj_picked" cPicked :: IO CInt
foreign import ccall unsafe "hj_scene_end" sceneEnd :: IO ()

initWindow :: String -> FilePath -> IO Bool
initWindow title path = withCString title $ \t -> withCString path $ \p -> (/=0) <$> cInit t p
shouldClose :: IO Bool
shouldClose = (/=0) <$> cShouldClose
dimensions :: IO (Float,Float)
dimensions = (,) <$> (fromIntegral <$> cWidth) <*> (fromIntegral <$> cHeight)
frameTime :: IO Float
frameTime = realToFrac <$> cDt
screenshot :: FilePath -> IO ()
screenshot path = withCString path cScreenshot
loadReplay :: FilePath -> IO Bool
loadReplay path = withCString path (fmap (/=0) . cReplay)
key :: Int -> Bool -> IO Bool
key k pressed = (/=0) <$> cKey (fromIntegral k) (if pressed then 1 else 0)
uiEnabled :: Bool -> IO ()
uiEnabled enabled = cUIEnabled (if enabled then 1 else 0)
inputs :: IO [Float]
inputs = allocaArray 4 $ \p -> cInputs p >> map realToFrac <$> peekArray 4 p
nativeError :: IO String
nativeError = cError >>= peekCString
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
focus v = withFloats (vlist v) cFocus
getCamera :: IO Camera
getCamera = allocaArray 7 $ \p -> cGetCamera p >> cameraFromData <$> (map realToFrac <$> peekArray 7 p)
cameraData :: Camera -> [Float]
cameraData c = vlist (eye c)++vlist (target c)++[fov c]
cameraFromData :: [Float] -> Camera
cameraFromData d = Camera (fromListV (take 3 d)) (fromListV (take 3 (drop 3 d))) (max 2 (min 178 (d!!6)))
withFloats :: [Float] -> (Ptr CFloat -> IO a) -> IO a
withFloats values = withArray (map realToFrac values)
f :: Float -> CFloat
f = realToFrac

drawScene :: FilePath -> Project -> (Float,Float,Float,Float) -> Bool -> Bool -> Maybe Entity -> IO (Maybe Entity)
drawScene root p (x,y,w,h) edit grid selected = do
  let env=environment p
  withFloats (cameraData (camera p)) $ \cam -> withFloats (vlist (sunDirection env)++vlist (sunColor env)++[sunPower env,ambient env,exposure env]) $ \en ->
    cSceneBegin (round x) (round y) (max 1 (round w)) (max 1 (round h)) (if edit then 1 else 0) cam en (if grid then 1 else 0)
  let ls=take 8 [(e,position t,l) | (e,l)<-imList (lights (world p)),Just t<-[imLookup e (transforms (world p))]]
  cLightCount (fromIntegral (length ls))
  forM_ (zip [0..] ls) $ \(i,(e,pos,l)) -> withFloats (vlist pos++vlist (lightColor l)++[intensity l]) $ \d -> do
    cLight i d
    when edit (cDrawLight d (if Just e==selected then 1 else 0))
  forM_ (renderables (world p)) $ \(e,t,m,mat) ->
    withFloats (componentData t mat) $ \d ->
    withCString (if geometry m `elem` builtins then geometry m else root </> geometry m) $ \mesh ->
    withCString (if null (texture m) then "" else root </> texture m) $ \tex -> cDrawEntity (fromIntegral e) d mesh tex (if Just e==selected then 1 else 0)
  picked<-fromIntegral <$> cPicked
  sceneEnd
  pure (if picked>0 then Just picked else selected)

-- Kept here to keep the native interface independent of the editor.
imList :: Data.IntMap.Strict.IntMap a -> [(Int,a)]
imList = Data.IntMap.Strict.toAscList
imLookup :: Int -> Data.IntMap.Strict.IntMap a -> Maybe a
imLookup = Data.IntMap.Strict.lookup
