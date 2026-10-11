-- Compiled independently by verify-reliability.py through the public library.
module Main where
import Unholy
import Data.IORef
import Control.Monad (when)
import System.Environment (getEnv)

main :: IO ()
main=do
  mode<-getEnv "UNHOLY_FAULT_MODE"
  armed<-newIORef True
  let failOnce phase=when (mode==phase) $ do
        first<-atomicModifyIORef' armed (\old->(False,old))
        when first (ioError (userError ("intentional "++phase++" failure")))
      before _ _ controls state p=do
        if mode=="step" then when (action controls) (failOnce "step") else failOnce "before"
        pure (state,p,[])
      after _ _ _ _ state p=do
        failOnce "after"
        first<-if mode=="hud" then atomicModifyIORef' armed (\old->(False,old)) else pure False
        pure (state,p,[HUD (if first then [head []] else "HEALTHY / editable scene preserved")])
      definition=game Game {identifier="fault-check",requiredAssets=[],components=[],validateScene=const (Right ()),
        initialize= \_ p->do
          putStrLn "FAULT-CHECK initialize"
          failOnce "initialize"
          pure ((),p,[])
        ,beforePhysics=before,afterPhysics=after}
  runApplication [definition]
