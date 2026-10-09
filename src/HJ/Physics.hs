{-# LANGUAGE ForeignFunctionInterface #-}
module HJ.Physics (Physics, createPhysics, closePhysics, stepPhysics) where

import HJ.Types
import HJ.Native (withFloats)
import qualified Data.IntMap.Strict as IM
import Foreign
import Foreign.C
import Control.Monad (forM_, foldM)

type Physics = Ptr ()
foreign import ccall unsafe "hj_physics_new" newPhysics :: IO Physics
foreign import ccall unsafe "hj_physics_close" closePhysics :: Physics -> IO ()
foreign import ccall unsafe "hj_physics_add" cAdd :: Physics -> CInt -> CInt -> CInt -> CFloat -> CFloat -> CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_physics_sync" cSync :: Physics -> CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hj_physics_step" cStep :: Physics -> CFloat -> IO ()
foreign import ccall unsafe "hj_physics_read" cRead :: Physics -> CInt -> Ptr CFloat -> IO ()

bodyData :: Transform -> Body -> [Float]
bodyData t b = concatMap vlist [position t,rotation t,scale t,velocity b]++[if grounded b then 1 else 0]
createPhysics :: World -> IO Physics
createPhysics w = do
  p<-newPhysics
  forM_ (IM.toAscList (bodies w)) $ \(e,b) -> case IM.lookup e (transforms w) of
    Nothing -> pure ()
    Just t -> withFloats (bodyData t b) $ cAdd p (fromIntegral e) (if bodyShape b==Sphere then 1 else 0) (if kinematic b then 1 else 0) (realToFrac (mass b)) (realToFrac (restitution b)) (realToFrac (friction b))
  pure p

stepPhysics :: Physics -> Float -> World -> IO World
stepPhysics p dt w = do
  forM_ (IM.toAscList (bodies w)) $ \(e,b) -> case IM.lookup e (transforms w) of
    Nothing->pure ()
    Just t->withFloats (bodyData t b) (cSync p (fromIntegral e))
  cStep p (realToFrac dt)
  foldM readBody w (IM.toAscList (bodies w))
  where
    readBody current (e,b) = case IM.lookup e (transforms current) of
      Nothing->pure current
      Just t->withFloats (bodyData t b) $ \d -> do
        cRead p (fromIntegral e) d
        xs<-map realToFrac <$> peekArray 13 d
        let tr=t {position=fromListV (take 3 xs),rotation=fromListV (take 3 (drop 3 xs))}
            body=b {velocity=fromListV (take 3 (drop 9 xs)),grounded=xs!!12>0.5}
        pure current {transforms=IM.insert e tr (transforms current),bodies=IM.insert e body (bodies current)}
