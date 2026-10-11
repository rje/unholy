module Unholy.Resource (Scope,newScope,register,acquire,closeScope) where

import Data.IORef
import Control.Exception (mask_,onException,finally)

newtype Scope = Scope (IORef (Maybe [IO ()]))
newScope :: IO Scope
newScope=Scope <$> newIORef (Just [])
register :: Scope -> IO () -> IO ()
register (Scope ref) release=do
  accepted<-atomicModifyIORef' ref $ \state->case state of
    Nothing->(Nothing,False)
    Just actions->(Just (release:actions),True)
  if accepted then pure () else ioError (userError "Game resource scope is closed")
acquire :: Scope -> IO a -> (a -> IO ()) -> IO a
acquire scope allocate release=mask_ $ do
  value<-allocate
  register scope (release value) `onException` release value
  pure value
closeScope :: Scope -> IO ()
closeScope (Scope ref)=mask_ $ do
  actions<-atomicModifyIORef' ref (\state->(Nothing,maybe [] id state))
  foldr finally (pure ()) actions
