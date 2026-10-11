module Main where
import Unholy
import Threshold
import qualified Data.IntMap.Strict as IM
import Control.Monad (foldM,unless)
import Control.Exception (finally)
import System.Directory (doesFileExist)
import Data.List (isInfixOf)

assert :: String -> Bool -> IO ()
assert label okay=unless okay (ioError (userError label))
neutral :: Input
neutral=Input 0 0 False False False
main :: IO ()
main=do
  let scene=sampleProject {world=(world sampleProject) {names=IM.map (const "Renamed") (names (world sampleProject))}}
  local<-doesFileExist "projects/threshold/project.hj.json"
  let root=if local then "projects/threshold" else "../../projects/threshold"
  (runtime,started,_)<-startRuntimeWith [threshold] Nothing root scene
  (do
    (finished,final,hud,walkedSlope,jumped)<-foldM (\(r,p,_,slopeSeen,jumpSeen) _->do
      let V3 _ _ z=position (transforms (world p) IM.! 2)
          controls=neutral {moveZ=(-1),jump=z<(-2.5) && z>(-5.5)}
      (next,q,errors,status)<-stepGame (1/120) controls r p
      assert (show errors) (not (any ("error" `isInfixOf`) errors))
      let V3 _ y z'=position (transforms (world q) IM.! 2)
      pure (next,q,status,slopeSeen || (z'>0 && z'<5 && y>1.2 && maybe False grounded (IM.lookup 2 (bodies (world q)))),jumpSeen || (z'<(-3) && y>4))) (runtime,started,"",False,False) [1..1000::Int]
    assert ("Traversal did not finish: "++hud++" / "++show (position (transforms (world final) IM.! 2))) ("THRESHOLD REACHED" `isInfixOf` hud)
    assert "Route did not exercise ramp contact and jump" (walkedSlope && jumped)
    let V3 _ _ crateZ=position (transforms (world final) IM.! 6)
    assert "Route did not push the convex crate" (abs (crateZ-7)>0.8)
    assert "Capsule tilted during traversal" (rotation (transforms (world final) IM.! 2)==V3 0 0 0)
    (resetRuntime,restored,_,_)<-stepGame (1/120) neutral {restart=True} finished final
    (_,reset,_,status)<-stepGame (1/120) neutral resetRuntime restored
    assert "Restart did not restore collectible bodies" (IM.size (bodies (world reset))==IM.size (bodies (world scene)))
    assert "Restart did not clear progress" ("0 of 3" `isInfixOf` status)) `finally` stopRuntime runtime
  (blockedRuntime,blockedStart,_)<-startRuntimeWith [threshold] Nothing root scene
  (do
    (_,blocked,hud)<-foldM (\(r,p,_) _->do
      (next,q,_,status)<-stepGame (1/120) neutral {moveZ=(-1)} r p
      pure (next,q,status)) (blockedRuntime,blockedStart,"") [1..1000::Int]
    let V3 _ _ z=position (transforms (world blocked) IM.! 2)
    assert "Raised threshold failed to require a jump" (z>(-5) && not ("THRESHOLD REACHED" `isInfixOf` hud))) `finally` stopRuntime blockedRuntime
  putStrLn "PASS Threshold: real capsule traversal, ramp mesh, convex push, jump, three triggers, completion and reset"
