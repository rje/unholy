module Unholy.ECS (Components, empty, insert, lookup, remove, query, prune, mapComponents) where

import Prelude hiding (lookup)
import qualified Data.IntMap.Strict as IM
import Unholy.Types (Entity,World,names)

-- Game-owned components remain typed and contiguous by entity ID. They live
-- in the game's state; scene components remain in World and serialize normally.
newtype Components a = Components (IM.IntMap a) deriving (Eq,Show)
empty :: Components a
empty=Components IM.empty
insert :: Entity -> a -> Components a -> Components a
insert e value (Components xs)=Components (IM.insert e value xs)
lookup :: Entity -> Components a -> Maybe a
lookup e (Components xs)=IM.lookup e xs
remove :: Entity -> Components a -> Components a
remove e (Components xs)=Components (IM.delete e xs)
query :: World -> Components a -> [(Entity,a)]
query w (Components xs)=IM.toAscList (IM.intersection xs (names w))
prune :: World -> Components a -> Components a
prune w (Components xs)=Components (IM.intersection xs (names w))
mapComponents :: (a -> b) -> Components a -> Components b
mapComponents f (Components xs)=Components (IM.map f xs)
