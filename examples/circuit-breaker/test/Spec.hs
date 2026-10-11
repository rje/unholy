module Main where
import Unholy
import CircuitBreaker
import qualified Data.IntMap.Strict as IM
import Control.Exception (finally)
import Control.Monad (unless,foldM)
import Data.List (isInfixOf)

assert :: String -> Bool -> IO ()
assert message good=unless good (ioError (userError message))
neutral :: Input
neutral=Input 0 0 False False False
main :: IO ()
main=do
  let renamed=sampleProject {world=(world sampleProject) {names=IM.map (const "Renamed") (names (world sampleProject))}}
  (runtime,p,_)<-startRuntimeWith [circuitBreaker] Nothing "." renamed
  (do
    (won,finished,hud)<-foldM (\(r,current,_) i->do
      (next,q,_,status)<-stepGame (1/120) neutral {moveZ=(-1),action=i `mod` 20<10} r current
      pure (next,q,status)) (runtime,p,"") [1..1100::Int]
    assert ("Puzzle failed: "++hud) ("CIRCUIT COMPLETE" `isInfixOf` hud)
    (r3,restored,_,_)<-stepGame (1/120) neutral {restart=True} won finished
    (_,reset,_,resetHud)<-stepGame (1/120) neutral r3 restored
    assert "Restart retains authored configuration" (authored (world reset)==authored (world renamed))
    assert "Restart clears circuit progress" ("0 of 2" `isInfixOf` resetHud)
    assert "Restart closes the completed puzzle's doors" (all (\e->IM.lookup e (transforms (world reset))==IM.lookup e (transforms (world renamed))) [4,7])) `finally` stopRuntime runtime
  putStrLn "PASS independent Circuit Breaker: renamed roles, real triggers/doors, completion and reset"
