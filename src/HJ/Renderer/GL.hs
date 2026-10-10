{-# LANGUAGE RecordWildCards #-}
-- The only renderer backend: direct OpenGL calls, with explicit ownership.
module HJ.Renderer.GL where

import Graphics.GL
import Foreign
import Foreign.C
import Control.Exception (bracket, bracketOnError, onException)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as VS
import Linear (M44, V3(..), V4(..))

data Program = Program { programId :: !GLuint, locations :: !(M.Map String GLint) }
data Target = Target { framebuffer :: !GLuint, colorTexture :: !GLuint, depthTexture :: !GLuint, targetWidth :: !Int, targetHeight :: !Int }
data Buffer = Buffer { vertexArray :: !GLuint, vertexBuffer :: !GLuint, indexBuffer :: !GLuint, elementCount :: !GLsizei, indexed :: !Bool }

genObject :: (GLsizei -> Ptr GLuint -> IO ()) -> IO GLuint
genObject gen = alloca $ \p -> gen 1 p >> peek p
deleteObject :: (GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> IO ()
deleteObject del ident = when (ident/=0) (with ident (del 1))

uniformNames :: [String]
uniformNames = ["mvp","matModel","matNormal","texture0","shadowMap","lightVP","baseColor","metalness","roughness","emission","viewPos","sunDirection","sunColor","sunPower","ambientPower","exposure","fogColor","fogDensity","useShadows","lightCount","skyTop","skyBottom","mode"]
  ++ [field++"["++show i++"]" | i<-[0..7::Int],field<-["lightPosition","lightColor","lightPower"]]

loadProgram :: FilePath -> FilePath -> IO Program
loadProgram vs fs = do
  vertex<-readFile vs
  fragment<-readFile fs
  compileProgram vs vertex fs fragment

compileProgram :: String -> String -> String -> String -> IO Program
compileProgram vLabel vertex fLabel fragment =
  bracket (compileShader GL_VERTEX_SHADER vLabel vertex) glDeleteShader $ \v ->
  bracket (compileShader GL_FRAGMENT_SHADER fLabel fragment) glDeleteShader $ \f ->
  bracketOnError glCreateProgram glDeleteProgram $ \p -> do
    glAttachShader p v
    glAttachShader p f
    forM_ [(0,"vertexPosition"),(1,"vertexTexCoord"),(2,"vertexNormal")] $ \(i,name) -> withCString name (glBindAttribLocation p i)
    glLinkProgram p
    okay<-getStatus (glGetProgramiv p) GL_LINK_STATUS
    unless okay $ do logText<-infoLog (glGetProgramiv p) (glGetProgramInfoLog p);ioError (userError ("Shader link failed: "++fLabel++"\n"++logText))
    glDetachShader p v
    glDetachShader p f
    locs<-forM uniformNames $ \n -> do l<-withCString n (glGetUniformLocation p);pure (n,l)
    pure (Program p (M.fromList locs))

compileShader :: GLenum -> String -> String -> IO GLuint
compileShader kind label source = bracketOnError (glCreateShader kind) glDeleteShader $ \s -> do
  withCString source $ \p -> with p $ \pp -> glShaderSource s 1 (castPtr pp) nullPtr
  glCompileShader s
  okay<-getStatus (glGetShaderiv s) GL_COMPILE_STATUS
  unless okay $ do logText<-infoLog (glGetShaderiv s) (glGetShaderInfoLog s);ioError (userError ("Shader compile failed: "++label++"\n"++logText))
  pure s

getStatus :: (GLenum -> Ptr GLint -> IO ()) -> GLenum -> IO Bool
getStatus get flag = alloca $ \p -> get flag p >> ((/=0) <$> peek p)
infoLog :: (GLenum -> Ptr GLint -> IO ()) -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> IO String
infoLog get logFn = alloca $ \n -> do
  get GL_INFO_LOG_LENGTH n
  size<-max 1 <$> peek n
  allocaBytes (fromIntegral size) $ \p -> logFn size nullPtr p >> peekCString p

location :: Program -> String -> GLint
location p name = M.findWithDefault (-1) name (locations p)
floatUniform :: Program -> String -> Float -> IO ()
floatUniform p n = glUniform1f (location p n)
intUniform :: Program -> String -> Int -> IO ()
intUniform p n = glUniform1i (location p n) . fromIntegral
vectorUniform :: Program -> String -> V3 Float -> IO ()
vectorUniform p n (V3 x y z) = glUniform3f (location p n) x y z
matrixUniform :: Program -> String -> M44 Float -> IO ()
matrixUniform p n m = with m $ \ptr -> glUniformMatrix4fv (location p n) 1 GL_TRUE (castPtr ptr)

uploadBuffer :: VS.Vector Float -> VS.Vector Word16 -> IO Buffer
uploadBuffer vertices indices = do
  vao<-genObject glGenVertexArrays
  vbo<-genObject glGenBuffers `onException` deleteObject glDeleteVertexArrays vao
  let cleanup=deleteObject glDeleteBuffers vbo >> deleteObject glDeleteVertexArrays vao
  ebo<-(if VS.null indices then pure 0 else genObject glGenBuffers) `onException` cleanup
  let buffer=Buffer vao vbo ebo (fromIntegral (if ebo==0 then VS.length vertices `div` 8 else VS.length indices)) (ebo/=0)
  (do
    glBindVertexArray vao
    glBindBuffer GL_ARRAY_BUFFER vbo
    VS.unsafeWith vertices $ \p -> glBufferData GL_ARRAY_BUFFER (fromIntegral (VS.length vertices*4)) (castPtr p) GL_STATIC_DRAW
    forM_ [(0,3,0),(1,2,12),(2,3,20)] $ \(attrib,count,offset) -> do
      glEnableVertexAttribArray attrib
      glVertexAttribPointer attrib count GL_FLOAT GL_FALSE 32 (intPtrToPtr offset)
    when (ebo/=0) $ do
      glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
      VS.unsafeWith indices $ \p -> glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral (VS.length indices*2)) (castPtr p) GL_STATIC_DRAW
    pure buffer) `onException` deleteBuffer buffer

drawBuffer :: GLenum -> Buffer -> IO ()
drawBuffer primitive Buffer{..} = do
  glBindVertexArray vertexArray
  if indexed then glDrawElements primitive elementCount GL_UNSIGNED_SHORT nullPtr else glDrawArrays primitive 0 elementCount
deleteBuffer :: Buffer -> IO ()
deleteBuffer Buffer{..} = do
  deleteObject glDeleteVertexArrays vertexArray
  deleteObject glDeleteBuffers vertexBuffer
  deleteObject glDeleteBuffers indexBuffer

newTexture :: Int -> Int -> Ptr a -> IO GLuint
newTexture w h pixels = bracketOnError (genObject glGenTextures) (deleteObject glDeleteTextures) $ \tex -> do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D tex
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA8) (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE (castPtr pixels)
  glGenerateMipmap GL_TEXTURE_2D
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
  pure tex

newTarget :: Bool -> Int -> Int -> IO Target
newTarget depthOnly w h = do
  fbo<-genObject glGenFramebuffers
  depth<-genObject glGenTextures `onException` deleteObject glDeleteFramebuffers fbo
  color<-(if depthOnly then pure 0 else genObject glGenTextures) `onException` (deleteObject glDeleteTextures depth >> deleteObject glDeleteFramebuffers fbo)
  let t=Target fbo color depth w h
  (do
    glBindFramebuffer GL_FRAMEBUFFER fbo
    forM_ [(depth,True),(color,False)] $ \(tex,isDepth) -> when (tex/=0) $ do
      glBindTexture GL_TEXTURE_2D tex
      glTexImage2D GL_TEXTURE_2D 0 (fromIntegral (if isDepth then GL_DEPTH_COMPONENT24 else GL_RGBA8)) (fromIntegral w) (fromIntegral h) 0 (if isDepth then GL_DEPTH_COMPONENT else GL_RGBA) (if isDepth then GL_UNSIGNED_INT else GL_UNSIGNED_BYTE) nullPtr
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral (if isDepth then GL_NEAREST else GL_LINEAR))
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral (if isDepth then GL_NEAREST else GL_LINEAR))
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_CLAMP_TO_EDGE)
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_CLAMP_TO_EDGE)
      glFramebufferTexture2D GL_FRAMEBUFFER (if isDepth then GL_DEPTH_ATTACHMENT else GL_COLOR_ATTACHMENT0) GL_TEXTURE_2D tex 0
    when depthOnly $ glDrawBuffer GL_NONE >> glReadBuffer GL_NONE
    status<-glCheckFramebufferStatus GL_FRAMEBUFFER
    unless (status==GL_FRAMEBUFFER_COMPLETE) (ioError (userError ("Incomplete renderer framebuffer: "++show status)))
    pure t) `onException` deleteTarget t

deleteTarget :: Target -> IO ()
deleteTarget Target{..} = do
  deleteObject glDeleteFramebuffers framebuffer
  deleteObject glDeleteTextures colorTexture
  deleteObject glDeleteTextures depthTexture
bindTarget :: Target -> IO ()
bindTarget Target{..} = glBindFramebuffer GL_FRAMEBUFFER framebuffer >> glViewport 0 0 (fromIntegral targetWidth) (fromIntegral targetHeight)

-- raygui and this renderer share a context. Flush UI first, then preserve every
-- OpenGL state that our passes/uploads change. The renderer never uses rlgl's
-- private matrix/shader caches, and returns the context exactly as it found it.
data GLState = GLState
  { savedDrawFbo :: !GLint, savedReadFbo :: !GLint, savedViewport :: !(V4 GLint)
  , savedProgram :: !GLint, savedVAO :: !GLint, savedVBO :: !GLint
  , savedActiveTexture :: !GLint, savedTex0 :: !GLint, savedTex1 :: !GLint
  , savedUnpack :: !GLint, savedPack :: !GLint, savedDepthFunc :: !GLint, savedCullFace :: !GLint
  , savedDepthMask :: !GLboolean, savedFlags :: ![(GLenum,GLboolean)]
  } deriving (Eq,Show)
getInt :: GLenum -> IO GLint
getInt name = alloca $ \p -> glGetIntegerv name p >> peek p
captureState :: IO GLState
captureState = do
  savedDrawFbo<-getInt GL_DRAW_FRAMEBUFFER_BINDING
  savedReadFbo<-getInt GL_READ_FRAMEBUFFER_BINDING
  savedViewport<-alloca $ \p -> glGetIntegerv GL_VIEWPORT (castPtr p) >> peek p
  savedProgram<-getInt GL_CURRENT_PROGRAM
  savedVAO<-getInt GL_VERTEX_ARRAY_BINDING
  savedVBO<-getInt GL_ARRAY_BUFFER_BINDING
  savedActiveTexture<-getInt GL_ACTIVE_TEXTURE
  glActiveTexture GL_TEXTURE0
  savedTex0<-getInt GL_TEXTURE_BINDING_2D
  glActiveTexture GL_TEXTURE1
  savedTex1<-getInt GL_TEXTURE_BINDING_2D
  glActiveTexture (fromIntegral savedActiveTexture)
  savedUnpack<-getInt GL_UNPACK_ALIGNMENT
  savedPack<-getInt GL_PACK_ALIGNMENT
  savedDepthFunc<-getInt GL_DEPTH_FUNC
  savedCullFace<-getInt GL_CULL_FACE_MODE
  savedDepthMask<-alloca $ \p -> glGetBooleanv GL_DEPTH_WRITEMASK p >> peek p
  savedFlags<-forM [GL_DEPTH_TEST,GL_CULL_FACE,GL_BLEND,GL_SCISSOR_TEST] $ \flag -> (,) flag <$> glIsEnabled flag
  pure GLState{..}
restoreState :: GLState -> IO ()
restoreState GLState{..} = do
  glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral savedDrawFbo)
  glBindFramebuffer GL_READ_FRAMEBUFFER (fromIntegral savedReadFbo)
  let V4 x y w h=savedViewport
  glViewport x y w h
  glUseProgram (fromIntegral savedProgram)
  glBindVertexArray (fromIntegral savedVAO)
  glBindBuffer GL_ARRAY_BUFFER (fromIntegral savedVBO)
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D (fromIntegral savedTex0)
  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D (fromIntegral savedTex1)
  glActiveTexture (fromIntegral savedActiveTexture)
  glPixelStorei GL_UNPACK_ALIGNMENT savedUnpack
  glPixelStorei GL_PACK_ALIGNMENT savedPack
  glDepthFunc (fromIntegral savedDepthFunc)
  glDepthMask savedDepthMask
  glCullFace (fromIntegral savedCullFace)
  forM_ savedFlags $ \(flag,enabled) -> if enabled==GL_TRUE then glEnable flag else glDisable flag

preservingState :: (GLState -> IO a) -> IO a
preservingState act = bracket captureState restoreState act
checkGL :: String -> IO ()
checkGL label = do
  err<-glGetError
  when (err/=GL_NO_ERROR) (ioError (userError (label++": OpenGL error "++show err)))
