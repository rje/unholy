module Main where

import Unholy
import PulseCourier
import qualified Data.IntMap.Strict as IM
import Control.Monad (unless,foldM)
import Control.Exception (finally)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (isInfixOf)

assert :: String -> Bool -> IO ()
assert message condition=unless condition (ioError (userError message))
neutral, forward :: Input
neutral=Input 0 0 False False False
forward=Input 0 (-1) False False False

main :: IO ()
main=do
  local<-doesDirectoryExist "projects/pulse-courier"
  let root=if local then "projects/pulse-courier" else "../../projects/pulse-courier"
  p<-loadProject (root </> projectFile) >>= either (ioError.userError) pure
  assert "Sample must use Haskell exclusively" (haskellGame p==Just "pulse-courier" && IM.null (scripts (world p)))
  assert "Unregistered game must fail explicitly" (either (const True) (const False) (findGame [] p))
  missing<-checkGameAssets [pulseCourier] root p
  assert (show missing) (null missing)
  (r,started,_)<-startRuntimeWith [pulseCourier] Nothing root p
  exercise root r started `finally` stopRuntime r
  stopRuntime r -- cleanup must be idempotent even after later world mutations
  putStrLn "PASS independently packaged Haskell game: components, rolling, jump, dash, all relays, finish, reset, edited scenes"
  where
    exercise root runtime initial=do
      ball<-case [e | (e,n)<-IM.toAscList (names (world initial)),n=="Courier"] of
        [e]->pure e
        _->ioError (userError "Missing Courier in sample scene")
      let height p=let V3 _ y _=position (transforms (world p) IM.! ball) in y
          advance n input r p=foldM (\(live,current,messages,status) _->do
            (next,q,logs,hud)<-stepGame (1/120) input live current
            pure (next,q,messages++logs,if null hud then status else hud)) (r,p,[],"") [1..n::Int]
      (settled,base,_,_)<-advance 60 neutral runtime initial
      assert "Ball must settle on the deck" (abs (height base-0.5)<0.08)
      (jumping,jumped,_,_)<-advance 15 neutral {jump=True} settled base
      assert "Haskell jump must reach Bullet" (height jumped>1)
      (reset,restored,_,_)<-advance 1 neutral {restart=True} jumping jumped
      (released,_,_,_)<-advance 1 neutral reset restored
      (dashing,dashed,_,_)<-advance 12 forward {action=True} released restored
      let V3 _ _ z=position (transforms (world dashed) IM.! ball)
      assert "Dash must move the body" (z<10.5)
      (finished,won,logs,hud)<-advance 650 forward dashing dashed
      assert ("Game did not finish: "++hud) ("DELIVERY COMPLETE" `isInfixOf` hud && any (isInfixOf "delivery complete") logs)
      assert "All relay entities must be removed" (not (any (isInfixOf "Relay ") (IM.elems (names (world won)))))
      (again,resetScene,_,_)<-advance 1 neutral {restart=True} finished won
      assert "Restart must restore the authored scene" (IM.size (names (world resetScene))==IM.size (names (world initial)))
      assert "Restart must restore the courier" (abs (height resetScene-height initial)<0.08)
      (_,_,_,status)<-advance 1 neutral again resetScene
      assert "Restart must clear custom player component state" ("0 of 3" `isInfixOf` status)
      let edited=initial {world=setTransform ball (identity {position=V3 1 0.6 11}) (world initial)}
      (newRuntime,newScene,_)<-startRuntimeWith [pulseCourier] Nothing root edited
      stopRuntime newRuntime
      assert "Game initialization must preserve editor changes" (position (transforms (world newScene) IM.! ball)==V3 1 0.6 11)
