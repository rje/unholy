{-# LANGUAGE ForeignFunctionInterface #-}
module Unholy.Physics
  ( Physics, Contact(..), CollisionEvent(..), RayHit(..), createPhysics, createPhysicsAt, closePhysics
  , configurePhysics, requestTeleport, synchronizePhysics, stepPhysics, collisionEvents, raycastPhysics, raycastSolid, bodyCount ) where

import Unholy.Types
import Unholy.Transform
import Unholy.Asset.CPU (loadTriangles)
import qualified Data.Vector.Storable as VS
import System.FilePath ((</>))
import Control.Exception (mask,mask_,bracketOnError,onException)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Linear as L
import Foreign
import Foreign.C
import Data.IORef
import Control.Monad (forM_, forM, unless, when)

data NativeBody = NativeBody { bodyHandle :: Ptr (), lastTransform :: Transform, lastBody :: Body, lastGeometry :: Maybe FilePath, lastTerrain :: Maybe Terrain }
data Contact = Contact { contactA :: Entity, contactB :: Entity, contactNormal :: V3, contactPoint :: V3, contactDistance :: Float, contactImpulse :: Float } deriving (Eq,Show)
data CollisionEvent = CollisionEvent { collisionKind :: String, collisionContact :: Contact, collisionTrigger :: Bool } deriving (Eq,Show)
data RayHit = RayHit { hitEntity :: Entity, hitPoint :: V3, hitNormal :: V3, hitFraction :: Float } deriving (Eq,Show)
data Physics = Physics { assetRoot :: FilePath, nativeWorld :: Ptr (), registry :: IORef (IM.IntMap NativeBody), settings :: IORef PhysicsSettings
  , previousPairs :: IORef (M.Map (Entity,Entity) (Contact,Bool)), events :: IORef [CollisionEvent], disposed :: IORef Bool, teleports :: IORef IS.IntSet }
foreign import ccall unsafe "hj_physics_new" cNew :: IO (Ptr ())
foreign import ccall unsafe "hj_physics_close" cClose :: Ptr () -> IO ()
foreign import ccall unsafe "hj_physics_gravity" cGravity :: Ptr () -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_body_new" cAdd :: Ptr () -> CInt -> CInt -> CInt -> CFloat -> CFloat -> CFloat -> Ptr CFloat -> Ptr Float -> CInt -> IO (Ptr ())
foreign import ccall unsafe "hj_body_close" cRemove :: Ptr () -> Ptr () -> IO ()
foreign import ccall unsafe "hj_body_scale" cScale :: Ptr () -> Ptr () -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_body_motion_pose" cMotion :: Ptr () -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_body_pose" cPose :: Ptr () -> Ptr () -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_body_clear" cClear :: Ptr () -> IO ()
foreign import ccall unsafe "hj_body_velocity" cVelocity :: Ptr () -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_body_read" cRead :: Ptr () -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_physics_step" cStep :: Ptr () -> CFloat -> IO ()
foreign import ccall unsafe "hj_physics_contacts" cContacts :: Ptr () -> Ptr CInt -> Ptr CFloat -> CInt -> IO CInt
foreign import ccall unsafe "hj_physics_ray" cRay :: Ptr () -> Ptr CFloat -> Ptr CFloat -> CInt -> CInt -> Ptr CFloat -> IO CInt
floats :: [Float] -> (Ptr CFloat -> IO a) -> IO a
floats xs=withArray (map realToFrac xs)
poseData :: Transform -> [Float]
poseData t = vlist (position t)++[x,y,z,w]
  where L.Quaternion w (L.V3 x y z)=rotationQ (rotation t)
colliderScale :: Transform -> Body -> V3
colliderScale t b = if bodyShape b==Sphere then let s=maximum (vlist (scale t)) in V3 s s s else scale t
createPhysics :: World -> IO Physics
createPhysics=createPhysicsAt "."
createPhysicsAt :: FilePath -> World -> IO Physics
createPhysicsAt root w=bracketOnError acquire closePhysics $ \p->do
  floats (vlist (gravity defaultPhysicsSettings)) (cGravity (nativeWorld p))
  synchronize p (resolveWorld w)
  pure p
  where acquire=do
          ptr<-cNew
          when (ptr==nullPtr) (ioError (userError "Cannot create Bullet world"))
          Physics root ptr <$> newIORef IM.empty <*> newIORef defaultPhysicsSettings <*> newIORef M.empty <*> newIORef [] <*> newIORef False <*> newIORef IS.empty
configurePhysics :: Physics -> PhysicsSettings -> IO ()
configurePhysics p config = do
  closed<-readIORef (disposed p)
  when closed (ioError (userError "Physics world is closed"))
  old<-readIORef (settings p)
  when (gravity old/=gravity config) (floats (vlist (gravity config)) (cGravity (nativeWorld p)))
  writeIORef (settings p) config
closePhysics :: Physics -> IO ()
closePhysics p = mask_ $ do
  closed<-atomicModifyIORef' (disposed p) (\x -> (True,x))
  unless closed $ do
    handles<-atomicModifyIORef' (registry p) (\x -> (IM.empty,x))
    mapM_ (cRemove (nativeWorld p) . bodyHandle) handles
    cClose (nativeWorld p)
bodyCount :: Physics -> IO Int
bodyCount p=IM.size <$> readIORef (registry p)
collisionEvents :: Physics -> IO [CollisionEvent]
collisionEvents=readIORef . events
requestTeleport :: Physics -> Entity -> IO ()
requestTeleport p e=modifyIORef' (teleports p) (IS.insert e)
synchronizePhysics :: Physics -> World -> IO ()
synchronizePhysics p = synchronize p . resolveWorld
synchronize :: Physics -> World -> IO ()
synchronize p w = mask $ \restore->do
  closed<-readIORef (disposed p)
  when closed (ioError (userError "Physics world is closed"))
  either (ioError . userError) pure (validateWorld w)
  old<-readIORef (registry p)
  resets<-readIORef (teleports p)
  let desired=IM.intersection (bodies w) (transforms w)
      geometryFor e b=if bodyShape b `elem` [Convex,TriangleMesh] then geometry <$> IM.lookup e (meshes w) else Nothing
      terrainFor e b=if bodyShape b==Heightfield then IM.lookup e (terrains w) else Nothing
      compatible e b native=configuration (lastBody native)==configuration b && lastGeometry native==geometryFor e b && lastTerrain native==terrainFor e b
      replacements=IM.filterWithKey (\e b->maybe True (not . compatible e b) (IM.lookup e old)) desired
      removed=IM.filterWithKey (\e _->not (IM.member e desired) || IM.member e replacements) old
  acquired<-newIORef IM.empty
  -- Loading a later file can fail. Keep the entire old registry alive until
  -- every replacement exists; release only the newly acquired bodies on error.
  fresh<-(IM.traverseWithKey (\e b->do
    value<-createBody restore e b (geometryFor e b)
    modifyIORef' acquired (IM.insert e value)
    pure value) replacements) `onException` (readIORef acquired >>= mapM_ (cRemove ptr . bodyHandle))
  let handles=IM.union fresh (IM.difference old removed)
  forM_ removed (cRemove ptr . bodyHandle)
  next<-IM.traverseWithKey (\e b->do
    let native=handles IM.! e;t=transforms w IM.! e;h=bodyHandle native;lastT=lastTransform native
    when (scale t/=scale lastT) (floats (vlist (colliderScale t b)) (cScale ptr h))
    if kinematic b && not (IS.member e resets) then floats (poseData t) (cMotion h) else
      when (IS.member e resets || position t/=position lastT || rotation t/=rotation lastT) $ do
        floats (poseData t) $ \d->cPose ptr h d >> cMotion h d
        cClear h
    when (IS.member e resets || velocity b/=velocity (lastBody native)) (floats (vlist (velocity b)) (cVelocity h))
    pure native {lastTransform=t,lastBody=b}) desired
  writeIORef (registry p) next
  modifyIORef' (teleports p) (`IS.difference` resets)
  where
    ptr=nativeWorld p
    configuration b=b {velocity=V3 0 0 0,grounded=False}
    createBody restore e b asset=do
      let terrain=if bodyShape b==Heightfield then IM.lookup e (terrains w) else Nothing
      points<-case terrain of
        Just value->pure (VS.fromList (terrainHeights value))
        Nothing->case asset of
          Nothing->pure VS.empty
          Just path->VS.unsafeCast <$> restore (loadTriangles (if path `elem` ["cube","sphere","plane","cylinder","torus","capsule"] then path else assetRoot p </> path))
      let t=transforms w IM.! e
          flags=(if kinematic b then 2 else if mass b==0 then 1 else 0)+(if trigger b then 4 else 0)
          ccd=if mass b>0 then [0.25,0.2*minimum (vlist (scale t))] else [0,0]
          data'=poseData t++vlist (colliderScale t b)++vlist (velocity b)++[linearDamping b,angularDamping b]++ccd++[if lockRotation b then 1 else 0]++maybe [0,0,0] (\v->[fromIntegral (terrainWidth v),fromIntegral (terrainDepth v),terrainSpacing v]) terrain
          count=if bodyShape b==Heightfield then VS.length points else VS.length points `div` 3
      h<-floats data' $ \d->VS.unsafeWith points $ \vertices->cAdd ptr (fromIntegral e) (fromIntegral (fromEnum (bodyShape b))) flags (realToFrac (mass b)) (realToFrac (restitution b)) (realToFrac (friction b)) d vertices (fromIntegral count)
      when (h==nullPtr) (ioError (userError ("Cannot create Bullet collider for entity "++show e)))
      pure (NativeBody h t b asset terrain)
stepPhysics :: Physics -> Float -> World -> IO World
stepPhysics p dt input = do
  let w=resolveWorld input
  synchronize p w
  cStep (nativeWorld p) (realToFrac dt)
  count<-fromIntegral <$> cContacts (nativeWorld p) nullPtr nullPtr 0
  contacts<-allocaArray (count*2) $ \ids -> allocaArray (count*8) $ \values -> do
    _<-cContacts (nativeWorld p) ids values (fromIntegral count)
    forM [0..count-1] $ \i -> do
      a<-fromIntegral <$> peekElemOff ids (i*2);b<-fromIntegral <$> peekElemOff ids (i*2+1)
      xs<-map realToFrac <$> peekArray 8 (values `advancePtr` (i*8))
      pure (Contact a b (fromListV (take 3 xs)) (fromListV (take 3 (drop 3 xs))) (xs!!6) (xs!!7))
  config<-readIORef (settings p)
  let touching=filter ((<=supportDistance config) . contactDistance) contacts
      isTrigger c=any (\e -> maybe False trigger (IM.lookup e (bodies w))) [contactA c,contactB c]
      pairs=M.fromListWith choose [(ordered c,(c,isTrigger c)) | c<-touching]
      ordered c=(min (contactA c) (contactB c),max (contactA c) (contactB c))
      choose a b=if contactDistance (fst a)<contactDistance (fst b) then a else b
      supported=IM.fromList [(e,()) | c<-touching,not (isTrigger c),let V3 _ ny _=contactNormal c,e<-[contactA c | ny>supportNormal config]++[contactB c | -ny>supportNormal config]]
  previous<-readIORef (previousPairs p)
  writeIORef (events p) ([CollisionEvent (if M.member pair previous then "stay" else "enter") c sensor | (pair,(c,sensor))<-M.toAscList pairs]
    ++[CollisionEvent "exit" c sensor | (_,(c,sensor))<-M.toAscList (M.difference previous pairs)])
  writeIORef (previousPairs p) pairs
  handles<-readIORef (registry p)
  rows<-IM.traverseWithKey (readBody supported w) handles
  writeIORef (registry p) rows
  pure (resolveWorld w {transforms=IM.union (IM.map lastTransform rows) (transforms w),bodies=IM.union (IM.map lastBody rows) (bodies w)})
  where
    readBody supported w e native = allocaArray 10 $ \d -> do
      cRead (bodyHandle native) d
      xs<-map realToFrac <$> peekArray 10 d
      let oldT=transforms w IM.! e
          q=L.Quaternion (xs!!6) (L.V3 (xs!!3) (xs!!4) (xs!!5))
          t=oldT {position=fromListV (take 3 xs),rotation=quaternionEuler q}
          b=(bodies w IM.! e) {velocity=fromListV (drop 7 xs),grounded=IM.member e supported}
      pure native {lastTransform=t,lastBody=b}
raycastPhysics :: Physics -> World -> V3 -> V3 -> Maybe Entity -> IO (Maybe RayHit)
raycastPhysics=raycastWith False
raycastSolid :: Physics -> World -> V3 -> V3 -> Maybe Entity -> IO (Maybe RayHit)
raycastSolid=raycastWith True
raycastWith :: Bool -> Physics -> World -> V3 -> V3 -> Maybe Entity -> IO (Maybe RayHit)
raycastWith solids p w from to ignore = do
  synchronize p (resolveWorld w)
  if from==to then pure Nothing else floats (vlist from) $ \a -> floats (vlist to) $ \b -> allocaArray 7 $ \d -> do
    e<-cRay (nativeWorld p) a b (maybe 0 fromIntegral ignore) (if solids then 1 else 0) d
    if e==0 then pure Nothing else do
      xs<-map realToFrac <$> peekArray 7 d
      pure (Just (RayHit (fromIntegral e) (fromListV (take 3 xs)) (fromListV (take 3 (drop 3 xs))) (xs!!6)))
