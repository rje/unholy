module Unholy.Terrain (generateTerrain, terrainMesh, spawnTerrain, setHeight) where

import Unholy.Types
import qualified Unholy.Renderer.Primitives as P
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import qualified Linear as L

-- Bounded patches fit one Word16-indexed buffer; there is no streaming/LOD.
generateTerrain :: Int -> Int -> Float -> (Float -> Float -> Float) -> Terrain
generateTerrain width depth spacing height=Terrain width depth spacing
  [height ((fromIntegral x-fromIntegral (width-1)/2)*spacing) ((fromIntegral z-fromIntegral (depth-1)/2)*spacing) | z<-[0..depth-1],x<-[0..width-1]]
spawnTerrain :: String -> Terrain -> World -> (Entity,World)
spawnTerrain name terrain w=let (e,next)=spawnMesh name "terrain" w
  in (e,next {terrains=IM.insert e terrain (terrains next),bodies=IM.insert e defaultBody {bodyShape=Heightfield} (bodies next)})
setHeight :: Int -> Float -> Terrain -> Terrain
setHeight index height terrain | index<0 || index>=length (terrainHeights terrain) || not (finite height) || abs height>10000=terrain
                              | otherwise=terrain {terrainHeights=take index values++[height]++drop (index+1) values}
  where values=terrainHeights terrain

terrainMesh :: Terrain -> Either String P.Primitive
terrainMesh t | not (validTerrain t)=Left "Invalid finite terrain grid"
              | otherwise=Right (P.Primitive vertices indices)
  where
    width=terrainWidth t;depth=terrainDepth t;spacing=terrainSpacing t
    heights=VS.fromList (terrainHeights t)
    height x z=heights VS.! (z*width+x)
    point x z=L.V3 ((fromIntegral x-fromIntegral (width-1)/2)*spacing) (height x z) ((fromIntegral z-fromIntegral (depth-1)/2)*spacing)
    normal x z=
      let left=max 0 (x-1);right=min (width-1) (x+1);back=max 0 (z-1);front=min (depth-1) (z+1)
          dx=(height right z-height left z)/(fromIntegral (right-left)*spacing)
          dz=(height x front-height x back)/(fromIntegral (front-back)*spacing)
      in L.normalize (L.V3 (-dx) 1 (-dz))
    vertices=VS.fromList (concat [let L.V3 px py pz=point x z;L.V3 nx ny nz=normal x z in [px,py,pz,px/3,pz/3,nx,ny,nz] | z<-[0..depth-1],x<-[0..width-1]])
    -- Same diagonal as Bullet flipQuadEdges=true: (x,z) to (x+1,z+1).
    indices=VS.fromList (concat [let a=fromIntegral (z*width+x);b=a+1;c=a+fromIntegral width;d=c+1 in [a,c,d,a,d,b] | z<-[0..depth-2],x<-[0..width-2]])
