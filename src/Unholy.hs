-- Public game-authoring API. See examples/pulse-courier for a separate package.
module Unholy
  ( module Unholy.Engine, module Unholy.Types, module Unholy.Transform, module Unholy.Game
  , module Unholy.Input, module Unholy.Physics
  , stepGame, Runtime, startRuntimeWith, stepRuntime, stopRuntime, runApplication
  , loadProject, saveProject, buildProject, buildProjectWithAssets, validateProject, projectFile ) where

import Unholy.Engine
import Unholy.Types
import Unholy.Transform
import Unholy.Game
import Unholy.Input
import Unholy.Physics
import Unholy.Script (Runtime,startRuntimeWith,stepRuntime,stopRuntime)
import Unholy.Application (runApplication)
import Unholy.Project (loadProject,saveProject,buildProject,buildProjectWithAssets,validateProject,projectFile)

-- Typed entrypoint for headless Haskell games; the editor/player use this same
-- runtime. The list-based stepRuntime remains available for older integrations.
stepGame :: Float -> Input -> Runtime -> Project -> IO (Runtime,Project,[String],String)
stepGame dt controls=stepRuntime dt (inputValues controls)
