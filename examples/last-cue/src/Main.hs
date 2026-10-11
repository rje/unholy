module Main where

import LastCue
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Unholy

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--write-scenes", root] -> do
      createDirectoryIfMissing True root
      saveProject (root </> projectFile) sampleProject >>= either (ioError . userError) pure
      saveProject (root </> "dock.hj.json") sampleProject >>= either (ioError . userError) pure
      saveProject (root </> "flies.hj.json") secondProject >>= either (ioError . userError) pure
    _ -> runApplication [lastCue]
