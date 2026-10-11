module Unholy.Renderer.Camera where

import qualified Unholy.Types as T
import Linear hiding (distance, point)
import qualified Data.Vector.Storable as VS
import Data.Word (Word16)

data Orbit = Orbit { orbitTarget :: !(V3 Float), orbitYaw :: !Float, orbitPitch :: !Float, orbitDistance :: !Float } deriving (Eq,Show)
data Mouse = Mouse { mouseX :: !Float, mouseY :: !Float, deltaX :: !Float, deltaY :: !Float, mouseWheel :: !Float, rightDown :: !Bool, middleDown :: !Bool, leftPressed :: !Bool, mouseEnabled :: !Bool } deriving (Eq,Show)
type Bounds = (V3 Float,V3 Float)
data Ray = Ray { rayOrigin :: !(V3 Float), rayDirection :: !(V3 Float) } deriving (Eq,Show)

defaultOrbit :: Orbit
defaultOrbit = Orbit (V3 0 1 0) 0.74 0.48 17.5
toLinear :: T.V3 -> V3 Float
toLinear (T.V3 x y z) = V3 x y z
fromLinear :: V3 Float -> T.V3
fromLinear (V3 x y z) = T.V3 x y z

inside :: (Float,Float,Float,Float) -> Mouse -> Bool
inside (x,y,w,h) m = mouseX m>=x && mouseX m<x+w && mouseY m>=y && mouseY m<y+h
updateOrbit :: (Float,Float,Float,Float) -> Mouse -> Orbit -> Orbit
updateOrbit rect m o
  | not (mouseEnabled m && inside rect m) = o
  | otherwise = o {orbitYaw=yaw,orbitPitch=pitch,orbitDistance=distance,orbitTarget=pan}
  where
    yaw=orbitYaw o-if rightDown m then deltaX m*0.006 else 0
    pitch=clamp (-1.45) 1.45 (orbitPitch o+if rightDown m then deltaY m*0.006 else 0)
    distance=clamp 1 300 (orbitDistance o*0.88**mouseWheel m)
    pan=if middleDown m then orbitTarget o+(-deltaX m*orbitDistance o*0.0013)*^V3 (cos yaw) 0 (-sin yaw)+V3 0 (deltaY m*orbitDistance o*0.0013) 0 else orbitTarget o
orbitCamera :: Orbit -> T.Camera
orbitCamera o = T.Camera (fromLinear (orbitTarget o+offset)) (fromLinear (orbitTarget o)) 45
  where offset=orbitDistance o*^V3 (sin (orbitYaw o)*cos (orbitPitch o)) (sin (orbitPitch o)) (cos (orbitYaw o)*cos (orbitPitch o))
clamp :: Float -> Float -> Float -> Float
clamp lo hi = max lo . min hi

viewProjection :: Float -> T.Camera -> M44 Float
viewProjection aspect c = perspective (T.fov c*pi/180) aspect 0.01 1000 !*! lookAt (toLinear (T.eye c)) (toLinear (T.target c)) (V3 0 1 0)
modelMatrix :: T.Transform -> M44 Float -> M44 Float
modelMatrix t imported = mkTransformation rotation (toLinear (T.position t)) !*! scaled (V4 sx sy sz 1) !*! imported
  where
    V3 sx sy sz=toLinear (T.scale t)
    V3 x y z=(pi/180)*^toLinear (T.rotation t)
    rotation=axisAngle (V3 0 0 1) z*axisAngle (V3 0 1 0) y*axisAngle (V3 1 0 0) x

sunMatrix :: T.Camera -> T.Environment -> M44 Float
sunMatrix cam env = ortho (-26) 26 (-26) 26 0.01 1000 !*! lookAt (point-45*^direction) point up
  where
    T.V3 x _ z=T.target cam
    point=V3 x 0 z
    d=toLinear (T.sunDirection env)
    direction=if quadrance d<1e-8 then normalize (V3 (-0.5) (-1) (-0.4)) else normalize d
    V3 _ dy _=direction
    up=if abs dy>0.99 then V3 0 0 1 else V3 0 1 0

transformPoint :: M44 Float -> V3 Float -> V3 Float
transformPoint m (V3 x y z) = let V4 a b c w=m !* V4 x y z 1 in V3 (a/w) (b/w) (c/w)
transformDirection :: M44 Float -> V3 Float -> V3 Float
transformDirection m (V3 x y z) = let V4 a b c _=m !* V4 x y z 0 in V3 a b c
screenRay :: (Float,Float,Float,Float) -> T.Camera -> Mouse -> Ray
screenRay (x,y,w,h) cam m = Ray origin (normalize (farPoint-origin))
  where
    origin=toLinear (T.eye cam)
    inverse=inv44 (viewProjection (w/h) cam)
    farPoint=transformPoint inverse (V3 (2*(mouseX m-x)/w-1) (1-2*(mouseY m-y)/h) 1)

boundsOf :: VS.Vector (V3 Float) -> Bounds
boundsOf points = VS.foldl' grow (VS.head points,VS.head points) points
  where grow (lo,hi) v = (liftI2 min lo v,liftI2 max hi v)
boundsCorners :: Bounds -> [V3 Float]
boundsCorners (V3 lx ly lz,V3 hx hy hz) = [V3 x y z | x<-[lx,hx],y<-[ly,hy],z<-[lz,hz]]
transformBounds :: M44 Float -> Bounds -> Bounds
transformBounds m b = boundsOf (VS.fromList (map (transformPoint m) (boundsCorners b)))

-- A ray transformed into object space keeps its unnormalised direction: its
-- parameter still measures world distance, including under nonuniform scale.
objectRay :: M44 Float -> Ray -> Ray
objectRay model (Ray p d) = Ray (transformPoint inverse p) (transformDirection inverse d)
  where inverse=inv44 model
rayBounds :: Ray -> Bounds -> Bool
rayBounds (Ray (V3 px py pz) (V3 dx dy dz)) (V3 lx ly lz,V3 hx hy hz) =
  case traverse interval [(px,dx,lx,hx),(py,dy,ly,hy),(pz,dz,lz,hz)] of
    Nothing -> False
    Just spans -> maximum (0:map fst spans)<=minimum (map snd spans)
  where
    interval (p,d,lo,hi)
      | abs d<1e-8 = if p<lo || p>hi then Nothing else Just (-1/0,1/0)
      | otherwise = let a=(lo-p)/d;b=(hi-p)/d in Just (min a b,max a b)
rayTriangle :: Ray -> V3 Float -> V3 Float -> V3 Float -> Maybe Float
rayTriangle (Ray origin direction) a b c
  | abs determinant<1e-8 || u<0 || u>1 || v<0 || u+v>1 || t<0 = Nothing
  | otherwise = Just t
  where
    e1=b-a;e2=c-a
    p=cross direction e2
    determinant=dot e1 p
    s=origin-a
    u=dot s p/determinant
    q=cross s e1
    v=dot direction q/determinant
    t=dot e2 q/determinant
rayMesh :: Ray -> Bounds -> VS.Vector (V3 Float) -> VS.Vector Word16 -> Maybe Float
rayMesh ray bounds vertices indices
  | not (rayBounds ray bounds) = Nothing
  | otherwise = foldl' test Nothing [0,3..count-3]
  where
    count=if VS.null indices then VS.length vertices else VS.length indices
    vertex i=vertices VS.! (if VS.null indices then i else fromIntegral (indices VS.! i))
    test best i=case rayTriangle ray (vertex i) (vertex (i+1)) (vertex (i+2)) of
      Nothing->best
      Just t->Just (maybe t (min t) best)
