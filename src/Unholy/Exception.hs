module Unholy.Exception (trySync) where

import Control.Exception (SomeException, SomeAsyncException, fromException, tryJust)

-- Gameplay failures are recoverable in the editor. Cancellation and process
-- shutdown retain their normal asynchronous exception semantics.
trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust synchronous
  where
    synchronous e = case fromException e :: Maybe SomeAsyncException of
      Just _ -> Nothing
      Nothing -> Just e
