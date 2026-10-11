module Unholy.Engine (Engine, withEngine, nextFrame, drawFrame, shouldClose, engineAudio) where

import Unholy.Types
import Unholy.Input (Input)
import qualified Unholy.Native as N
import qualified Unholy.Audio as Audio
import Control.Concurrent (runInBoundThread)

-- raylib's platform backend supports one window/device at a time. Resource
-- ownership is explicit; creating and using this Engine stays on its OS thread.
newtype Engine = Engine N.Window
withEngine :: String -> FilePath -> (Engine -> IO a) -> IO a
withEngine title shaders action=runInBoundThread (N.withWindow title shaders (action . Engine))
nextFrame :: Engine -> IO (Float,Input)
nextFrame (Engine window)=(,) <$> N.frameTime window <*> N.input window
shouldClose :: Engine -> IO Bool
shouldClose (Engine window)=N.shouldClose window
engineAudio :: Engine -> Maybe Audio.Engine
engineAudio (Engine window)=N.audio window
drawFrame :: Engine -> FilePath -> Project -> IO ()
drawFrame (Engine window) root p=do
  (w,h)<-N.dimensions window
  N.begin window
  _<-N.drawScene window root p (0,0,w,h) False False Nothing
  N.end window
