module Unholy.Input (Input(..), FrameInput(..), emptyFrame, gameInput, inputValues, keyHeld, keyPressed) where

import qualified Data.IntSet as S
import Unholy.Renderer.Camera (Mouse(..))

data Input = Input { moveX :: !Float, moveZ :: !Float, jump :: !Bool, restart :: !Bool, action :: !Bool }
  deriving (Eq,Show)
data FrameInput = FrameInput
  { heldKeys :: !S.IntSet, pressedKeys :: !S.IntSet, pointer :: !Mouse
  , leftDown :: !Bool, leftReleased :: !Bool }
emptyFrame :: FrameInput
emptyFrame=FrameInput S.empty S.empty (Mouse 0 0 0 0 0 False False False True) False False
keyHeld, keyPressed :: Int -> FrameInput -> Bool
keyHeld k=S.member k . heldKeys
keyPressed k=S.member k . pressedKeys
gameInput :: Bool -> FrameInput -> Input
gameInput focused _ | focused=Input 0 0 False False False
gameInput _ frame=Input (axis [68,262] [65,263]) (axis [83,264] [87,265]) (held 32) (held 82) (held 69)
  where held k=keyHeld k frame;axis pos neg=fromIntegral (fromEnum (any held pos)-fromEnum (any held neg))
inputValues :: Input -> [Float]
inputValues i=[moveX i,moveZ i,b (jump i),b (restart i),b (action i)]
  where b x=if x then 1 else 0
