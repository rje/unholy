{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
-- Radiance decoding is reused; Haskell owns uploads, convolution and lifetime.
module Unholy.Renderer.IBL where

import Unholy.Renderer.GL
import Graphics.GL
import Foreign
import Foreign.C
import qualified Data.Vector.Storable as VS
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forM_, unless)

data HDR = HDR { hdrWidth :: !Int, hdrHeight :: !Int, hdrPixels :: !(VS.Vector Float) }
data IBL = IBL { radianceTexture :: !GLuint, irradianceTexture :: !GLuint, prefilterTexture :: !GLuint, brdfTexture :: !GLuint } deriving (Eq,Show)

foreign import ccall unsafe "hj_hdr_load" cHDR :: CString -> Ptr CInt -> IO (Ptr Float)
foreign import ccall unsafe "hj_hdr_free" cFree :: Ptr Float -> IO ()

loadHDR :: FilePath -> IO HDR
loadHDR path = withCString path $ \name -> allocaArray 2 $ \size -> bracket (cHDR name size) cFree $ \p -> do
  unless (p/=nullPtr) (ioError (userError ("Cannot decode HDR environment: "++path)))
  w<-fromIntegral <$> peekElemOff size 0
  h<-fromIntegral <$> peekElemOff size 1
  unless (w>0 && h>0 && w<=8192 && h<=4096) (ioError (userError "HDR environment dimensions exceed 8192 x 4096"))
  storage<-mallocForeignPtrArray (w*h*4)
  withForeignPtr storage $ \out->copyArray out p (w*h*4)
  let values=VS.unsafeFromForeignPtr0 storage (w*h*4)
  unless (VS.all (\v->not (isNaN v || isInfinite v) && v>=0 && v<=60000) values) (ioError (userError "Invalid HDR radiance samples"))
  pure (HDR w h values)

deleteIBL :: IBL -> IO ()
deleteIBL IBL{..}=mapM_ (deleteObject glDeleteTextures) [radianceTexture,irradianceTexture,prefilterTexture,brdfTexture]

loadIBL :: Program -> Buffer -> FilePath -> IO IBL
loadIBL program screen path=loadHDR path >>= generateIBL program screen

generateIBL :: Program -> Buffer -> HDR -> IO IBL
generateIBL program screen hdr@HDR{..}=validateHDR hdr >> preservingState (\_ ->
  bracketOnError (genObject glGenTextures) (deleteObject glDeleteTextures) $ \radianceTexture ->
  bracketOnError (cubeTexture 32 1) (deleteObject glDeleteTextures) $ \irradianceTexture ->
  bracketOnError (cubeTexture 128 7) (deleteObject glDeleteTextures) $ \prefilterTexture ->
  bracketOnError (genObject glGenTextures) (deleteObject glDeleteTextures) $ \brdfTexture ->
  bracket (genObject glGenFramebuffers) (deleteObject glDeleteFramebuffers) $ \fbo -> do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D radianceTexture
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    VS.unsafeWith hdrPixels $ \p->glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA16F) (fromIntegral hdrWidth) (fromIntegral hdrHeight) 0 GL_RGBA GL_FLOAT (castPtr p)
    glGenerateMipmap GL_TEXTURE_2D
    parameters GL_TEXTURE_2D GL_LINEAR_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glBindTexture GL_TEXTURE_2D brdfTexture
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RG16F) 128 128 0 GL_RG GL_FLOAT nullPtr
    parameters GL_TEXTURE_2D GL_LINEAR
    glBindTexture GL_TEXTURE_2D radianceTexture
    glBindFramebuffer GL_FRAMEBUFFER fbo
    glDrawBuffer GL_COLOR_ATTACHMENT0
    glReadBuffer GL_COLOR_ATTACHMENT0
    glDisable GL_DEPTH_TEST
    glDisable GL_CULL_FACE
    glDisable GL_BLEND
    glDisable GL_SCISSOR_TEST
    glUseProgram (programId program)
    intUniform program "environmentMap" 0
    let render target texture level size mode = do
          glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 target texture level
          complete<-glCheckFramebufferStatus GL_FRAMEBUFFER
          unless (complete==GL_FRAMEBUFFER_COMPLETE) (ioError (userError "Incomplete IBL convolution framebuffer"))
          glViewport 0 0 size size
          intUniform program "mode" mode
          drawBuffer GL_TRIANGLES screen
    forM_ [0..5] $ \face->do
      intUniform program "face" face
      render (GL_TEXTURE_CUBE_MAP_POSITIVE_X+fromIntegral face) irradianceTexture 0 32 0
      forM_ [0..6::Int] $ \level->do
        floatUniform program "roughnessLevel" (fromIntegral level/6)
        render (GL_TEXTURE_CUBE_MAP_POSITIVE_X+fromIntegral face) prefilterTexture (fromIntegral level) (128 `div` (2^level)) 1
    render GL_TEXTURE_2D brdfTexture 0 128 2
    checkGL "HDR environment convolution"
    pure IBL{..})

validateHDR :: HDR -> IO ()
validateHDR HDR{..} = do
  unless (hdrWidth>0 && hdrHeight>0 && hdrWidth<=8192 && hdrHeight<=4096 && VS.length hdrPixels==hdrWidth*hdrHeight*4) (ioError (userError "Invalid HDR dimensions or pixel buffer length"))
  unless (VS.all (\v->not (isNaN v || isInfinite v) && v>=0 && v<=60000) hdrPixels) (ioError (userError "Invalid HDR radiance samples"))

cubeTexture :: Int -> Int -> IO GLuint
cubeTexture size levels=bracketOnError (genObject glGenTextures) (deleteObject glDeleteTextures) $ \tex->do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_CUBE_MAP tex
  forM_ [0..levels-1] $ \level->forM_ [0..5::Int] $ \face->do
    let side=fromIntegral (size `div` (2^level))
    glTexImage2D (GL_TEXTURE_CUBE_MAP_POSITIVE_X+fromIntegral face) (fromIntegral level) (fromIntegral GL_RGB16F) side side 0 GL_RGB GL_FLOAT nullPtr
  parameters GL_TEXTURE_CUBE_MAP (if levels==1 then GL_LINEAR else GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MAX_LEVEL (fromIntegral (levels-1))
  pure tex

parameters :: GLenum -> GLenum -> IO ()
parameters target minFilter=do
  glTexParameteri target GL_TEXTURE_MIN_FILTER (fromIntegral minFilter)
  glTexParameteri target GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  forM_ [GL_TEXTURE_WRAP_S,GL_TEXTURE_WRAP_T,GL_TEXTURE_WRAP_R] $ \axis->glTexParameteri target axis (fromIntegral GL_CLAMP_TO_EDGE)
