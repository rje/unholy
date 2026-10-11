-- Games own the schema. Player progress never rewrites an authored Project.
module Unholy.Checkpoint (readCheckpoint, writeCheckpoint) where

import Control.Exception (bracketOnError, mask)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.FilePath
import System.IO
import Unholy.Exception (trySync)

readCheckpoint :: (FromJSON a) => FilePath -> IO (Either String (Maybe a))
readCheckpoint path = do
  result <- trySync $ do
    exists <- doesFileExist path
    if not exists
      then pure (Right Nothing)
      else do
        size <- withFile path ReadMode hFileSize
        if size > 1048576 then pure (Left "Checkpoint is too large") else fmap Just . eitherDecodeStrict' <$> B.readFile path
  pure (either (Left . show) id result)

writeCheckpoint :: (ToJSON a) => FilePath -> a -> IO (Either String ())
writeCheckpoint path value = do
  result <- trySync $ mask $ \restore -> do
    let directory = takeDirectory path
    createDirectoryIfMissing True directory
    bracketOnError
      (openBinaryTempFile directory ".checkpoint.tmp")
      (\(temporary, handle) -> void (trySync (hClose handle)) >> void (trySync (removeFile temporary)))
      ( \(temporary, handle) -> do
          restore (BL.hPut handle (encode value) >> hClose handle)
          renameFile temporary path
      )
  pure (either (Left . show) Right result)
