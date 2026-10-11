-- CPU geometry is generated without a window or a temporary native model.
module Unholy.Renderer.Primitives (Primitive(..), primitive) where

import Linear (V3(..), cross, dot, norm, normalize)
import qualified Data.Vector.Storable as V
import Data.Word (Word16)

data Primitive = Primitive { vertices :: V.Vector Float, indices :: V.Vector Word16 }
type Vertex = (V3 Float,(Float,Float),V3 Float)

primitive :: String -> Maybe Primitive
primitive name = pack <$> case name of
  "cube" -> Just (concatMap face [(V3 1 0 0,V3 0 0 (-1),V3 0 1 0),(V3 (-1) 0 0,V3 0 0 1,V3 0 1 0),(V3 0 1 0,V3 1 0 0,V3 0 0 (-1)),(V3 0 (-1) 0,V3 1 0 0,V3 0 0 1),(V3 0 0 1,V3 1 0 0,V3 0 1 0),(V3 0 0 (-1),V3 (-1) 0 0,V3 0 1 0)])
  "plane" -> Just (quad [(V3 (-0.5) 0 (-0.5),(0,0),up),(V3 (-0.5) 0 0.5,(0,1),up),(V3 0.5 0 0.5,(1,1),up),(V3 0.5 0 (-0.5),(1,0),up)])
  "sphere" -> Just (surface 32 32 sphere)
  "cylinder" -> Just (surface 32 1 cylinder ++ concat [cap y n i | (y,n)<-[(0,V3 0 (-1) 0),(1,up)],i<-[0..31::Int]])
  "capsule" -> Just (surface 32 16 (capsuleCap True) ++ surface 32 16 (capsuleCap False) ++ surface 32 1 capsuleSide)
  "torus" -> Just (surface 32 24 torus)
  _ -> Nothing
  where
    up=V3 0 1 0
    face (n,u,v)=quad [(fmap (*0.5) (n+fmap (*a) u+fmap (*b) v),((a+1)/2,(1-b)/2),n) | (a,b)<-[(-1,-1),(1,-1),(1,1),(-1,1)]]
    sphere u v=let a=2*pi*u;b=pi*v;n=V3 (sin b*cos a) (cos b) (sin b*sin a) in (fmap (*0.5) n,(u,v),n)
    cylinder u v=let a=2*pi*u;n=V3 (cos a) 0 (sin a) in (V3 (0.5*cos a) v (0.5*sin a),(u,1-v),n)
    capsuleCap upper u v=let a=2*pi*u;b=(if upper then v else 1+v)*pi/2;n=V3 (sin b*cos a) (cos b) (sin b*sin a);V3 x y z=fmap (*0.5) n
                         in (V3 x (y+if upper then 0.5 else -0.5) z,(u,v),n)
    capsuleSide u v=let a=2*pi*u;n=V3 (cos a) 0 (sin a) in (V3 (0.5*cos a) (v-0.5) (0.5*sin a),(u,v),n)
    -- Preserve the existing torus: XY ring, major radius .4, tube radius .08.
    torus u v=let a=2*pi*u;b=2*pi*v;r=0.4+0.08*cos b;n=V3 (cos b*cos a) (cos b*sin a) (sin b) in (V3 (r*cos a) (r*sin a) (0.08*sin b),(u,v),n)
    cap y n i=let a=2*pi*fromIntegral i/32;b=2*pi*fromIntegral (i+1)/32
                  point t=(V3 (0.5*cos t) y (0.5*sin t),((cos t+1)/2,(sin t+1)/2),n)
              in orient [(V3 0 y 0,(0.5,0.5),n),point a,point b]

surface :: Int -> Int -> (Float -> Float -> Vertex) -> [Vertex]
surface nu nv f=concat [quad [f u v,f u' v,f u' v',f u v'] | i<-[0..nu-1],j<-[0..nv-1],let u=fromIntegral i/fromIntegral nu;u'=fromIntegral (i+1)/fromIntegral nu;v=fromIntegral j/fromIntegral nv;v'=fromIntegral (j+1)/fromIntegral nv]
quad :: [Vertex] -> [Vertex]
quad [a,b,c,d]=orient [a,b,c]++orient [a,c,d]
quad _=[]
orient :: [Vertex] -> [Vertex]
orient [a@(p,_,n),b@(q,_,m),c@(r,_,o)]
  | norm normal<1e-7=[] -- Avoid zero-area triangles at sphere poles.
  | dot normal (n+m+o)<0=[a,c,b]
  | otherwise=[a,b,c]
  where normal=cross (q-p) (r-p)
orient _=[]
pack :: [Vertex] -> Primitive
pack vs=Primitive (V.fromList (concatMap flatten vs)) V.empty
  where flatten (V3 x y z,(u,v),normal)=let V3 a b c=normalize normal in [x,y,z,u,v,a,b,c]
