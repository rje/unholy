module Unholy.Prefab (Prefab(..), loadPrefab, savePrefab, makePrefab, instantiate) where

import Unholy.Types
import Unholy.Transform
import Unholy.Project
import Unholy.JSON
import qualified Data.IntMap.Strict as IM
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, (</>))

data Prefab = Prefab { prefabRoot :: Entity, prefabWorld :: World } deriving (Eq,Show)
loadPrefab :: FilePath -> FilePath -> IO (Either String Prefab)
loadPrefab root path = ioEither $ do
  either (ioError . userError) pure (assetPath path)
  source<-readFile (root </> path)
  j<-either (ioError . userError) pure (parseJSON source)
  rootNumber<-either (ioError . userError) pure (field "prefabRoot" j >>= number)
  unless (rootNumber>0 && rootNumber==fromIntegral (round rootNumber::Int)) (ioError (userError "Invalid prefab root"))
  let rootId=round rootNumber
  p<-either (ioError . userError) pure (decodeProject source)
  unless (IM.member rootId (names (world p)) && length (descendants rootId (world p))==IM.size (names (world p)))
    (ioError (userError "A prefab must contain one root and its descendants"))
  missing<-checkAssets root p
  unless (null missing) (ioError (userError (unlines missing)))
  pure (Prefab rootId (world p))
makePrefab :: Entity -> World -> Either String Prefab
makePrefab e input = do
  let w=resolveWorld input
  unless (IM.member e (names w)) (Left "Select an existing prefab root")
  let members=IM.fromList [(id',()) | id'<-descendants e w]
      keep table=IM.intersection table members
      rootPose=transforms w IM.! e
      subset=w {names=keep (names w),transforms=IM.insert e (rootPose {position=V3 0 0 0}) (keep (transforms w)),meshes=keep (meshes w),materials=keep (materials w),scripts=keep (scripts w),lights=keep (lights w),bodies=keep (bodies w),parents=IM.delete e (keep (parents w))}
  validateWorld subset
  pure (Prefab e (resolveWorld subset))
savePrefab :: FilePath -> Prefab -> IO (Either String ())
savePrefab path prefab = ioEither $ do
  either (ioError . userError) pure (validateWorld (prefabWorld prefab))
  createDirectoryIfMissing True (takeDirectory path)
  fields<-either (ioError . userError) pure (object (projectJSON ((emptyProject "Prefab") {world=prefabWorld prefab})))
  let temp=path++".tmp"
  writeFile temp (encodeJSON (Object (("prefabRoot",Number (fromIntegral (prefabRoot prefab))):fields))++"\n")
  renameFile temp path
instantiate :: Prefab -> Transform -> World -> Either String (Entity,[Entity],World)
instantiate prefab placement current = do
  let source=prefabWorld prefab; ids=IM.keys (names source)
  unless (IM.size (names current)+length ids<=4096) (Left "Runtime entity limit (4096) reached")
  let remap=IM.fromList (zip ids [nextId current..]);idOf e=remap IM.! e
      root=idOf (prefabRoot prefab)
      copy table old=IM.union (IM.fromList [(idOf e,v) | (e,v)<-IM.toAscList table]) old
      poses=IM.mapWithKey (\e t -> if e==prefabRoot prefab then compose placement t else t) (transforms source)
      parentCopies=IM.map (\p -> p {parentEntity=idOf (parentEntity p)}) (parents source)
      next=resolveWorld current {nextId=nextId current+length ids,names=copy (names source) (names current),transforms=copy poses (transforms current)
        ,meshes=copy (meshes source) (meshes current),materials=copy (materials source) (materials current),scripts=copy (scripts source) (scripts current)
        ,lights=copy (lights source) (lights current),bodies=copy (bodies source) (bodies current),parents=copy parentCopies (parents current)}
  validateWorld next
  pure (root,map idOf ids,next)
