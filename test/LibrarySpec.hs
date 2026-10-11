module LibrarySpec (primitiveGeometry, typedInputComponents) where

import Unholy
import qualified Unholy.ECS as ECS
import qualified Unholy.Renderer.Primitives as P
import qualified Unholy.Renderer.Camera as C
import qualified Data.Vector.Storable as V
import qualified Data.IntSet as S
import qualified Linear as L
import Control.Monad (unless,forM_)

assert :: String -> Bool -> IO ()
assert label good=unless good (ioError (userError label))
primitiveGeometry :: IO ()
primitiveGeometry=forM_ ["cube","plane","sphere","cylinder","torus"] $ \name->case P.primitive name of
  Nothing->assert (name++": missing") False
  Just cpu->do
    let data_=P.vertices cpu
        count=V.length data_ `div` 8
        vertex i=let o=i*8 in L.V3 (data_ V.! o) (data_ V.! (o+1)) (data_ V.! (o+2))
        normal i=let o=i*8+5 in L.V3 (data_ V.! o) (data_ V.! (o+1)) (data_ V.! (o+2))
        triangles=[(i,i+1,i+2) | i<-[0,3..count-3]]
        outward (a,b,c)=let n=L.cross (vertex b-vertex a) (vertex c-vertex a) in L.norm n>1e-7 && L.dot n (normal a+normal b+normal c)>0
        positions=V.generate count vertex
        (lo,hi)=C.boundsOf positions
    assert (name++": finite packed CPU triangles") (count>0 && count `mod` 3==0 && V.length data_==count*8 && V.all finite data_)
    assert (name++": outward triangle winding") (all outward triangles)
    assert (name++": unit normals") (all (\i->abs (L.norm (normal i)-1)<0.0001) [0..count-1])
    assert (name++": valid texture coordinates") (all (\i->all (\k->let x=data_ V.! (i*8+k) in x>=0 && x<=1) [3,4]) [0..count-1])
    let near a b=L.norm (a-b)<0.0001
    case name of
      "cube"->assert "Cube must match the unit Bullet box" (near lo (L.V3 (-0.5) (-0.5) (-0.5)) && near hi (L.V3 0.5 0.5 0.5))
      "sphere"->assert "Sphere must match the unit Bullet sphere" (near lo (L.V3 (-0.5) (-0.5) (-0.5)) && near hi (L.V3 0.5 0.5 0.5))
      "cylinder"->assert "Cylinder must keep its existing base at Y=0" (near lo (L.V3 (-0.5) 0 (-0.5)) && near hi (L.V3 0.5 1 0.5))
      "torus"->assert "Torus must keep its XY ring and original radii" (near lo (L.V3 (-0.48) (-0.48) (-0.08)) && near hi (L.V3 0.48 0.48 0.08))
      _->pure ()

typedInputComponents :: IO ()
typedInputComponents=do
  let input=emptyFrame {heldKeys=S.fromList [87,68,32,82,69]}
      expected=Input 1 (-1) True True True
  assert "Default keyboard bindings" (gameInput False input==expected)
  assert "Text focus suppresses game controls" (gameInput True input==Input 0 0 False False False)
  assert "Opposing bindings cancel" (moveX (gameInput False input {heldKeys=S.fromList [65,68]})==0)
  let (e,w)=spawn "Typed entity" emptyWorld
      components=ECS.insert e (42::Int) ECS.empty
  assert "Game-owned typed component lookup" (ECS.lookup e components==Just 42)
  assert "Queries ignore removed entities" (null (ECS.query (destroy e w) components))
  assert "Pruning releases removed components" (ECS.lookup e (ECS.prune (destroy e w) components)==Nothing)
