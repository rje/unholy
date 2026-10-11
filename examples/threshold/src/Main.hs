module Main where
import Unholy
import Threshold
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
main :: IO ()
main=do
  args<-getArgs
  case args of
    ["--write-scene",root]->do
      createDirectoryIfMissing True root
      saveProject (root </> projectFile) sampleProject >>= either (ioError.userError) pure
    _->runApplication [threshold]
