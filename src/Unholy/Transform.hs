module Unholy.Transform (resolveWorld, setTransform, compose, relative, rotationQ, quaternionEuler) where

import Unholy.Types
import qualified Data.IntMap.Strict as IM
import qualified Linear as L

rotationQ :: V3 -> L.Quaternion Float
rotationQ (V3 x y z) = L.axisAngle (L.V3 0 0 1) (rad z)*L.axisAngle (L.V3 0 1 0) (rad y)*L.axisAngle (L.V3 1 0 0) (rad x)
  where rad a=a*pi/180
quaternionEuler :: L.Quaternion Float -> V3
quaternionEuler q = V3 (atan2 m21 m22*k) (asin (max (-1) (min 1 (-m20)))*k) (atan2 m10 m00*k)
  where L.V3 (L.V3 m00 _ _) (L.V3 m10 _ _) (L.V3 m20 m21 m22)=L.fromQuaternion q;k=180/pi
vec :: V3 -> L.V3 Float
vec (V3 x y z)=L.V3 x y z
back :: L.V3 Float -> V3
back (L.V3 x y z)=V3 x y z
compose :: Transform -> Transform -> Transform
compose a b = Transform (back (vec (position a)+L.rotate (rotationQ (rotation a)) (vec (scale a)*vec (position b))))
  (quaternionEuler (rotationQ (rotation a)*rotationQ (rotation b))) (back (vec (scale a)*vec (scale b)))
relative :: Transform -> Transform -> Transform
relative a b = Transform (back (L.rotate inverse (vec (position b)-vec (position a))/vec (scale a)))
  (quaternionEuler (inverse*rotationQ (rotation b))) (back (vec (scale b)/vec (scale a)))
  where inverse=L.conjugate (rotationQ (rotation a))
resolveWorld :: World -> World
resolveWorld w = w {transforms=IM.mapWithKey (\e _ -> pose [] e) (transforms w)}
  where pose seen e | e `elem` seen = identity
                   | otherwise = case IM.lookup e (parents w) of
                       Nothing -> IM.findWithDefault identity e (transforms w)
                       Just p -> compose (pose (e:seen) (parentEntity p)) (localTransform p)
setTransform :: Entity -> Transform -> World -> World
setTransform e t w
  | IM.lookup e (transforms w)==Just t = w
  | otherwise = foldl refresh changed (drop 1 (descendants e w))
  where
    changed=w {transforms=IM.insert e t (transforms w),parents=IM.adjust update e (parents w)}
    update p = p {localTransform=relative (IM.findWithDefault identity (parentEntity p) (transforms w)) t}
    refresh current child = case IM.lookup child (parents current) of
      Nothing -> current
      Just p -> current {transforms=IM.insert child (compose (IM.findWithDefault identity (parentEntity p) (transforms current)) (localTransform p)) (transforms current)}
