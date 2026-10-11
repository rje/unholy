module Unholy.Input (Input(..), FrameInput(..), Gamepad(..), neutralGamepad, radialStick, emptyFrame, gameInput, inputValues, keyHeld, keyPressed) where

import qualified Data.IntSet as S
import Unholy.Renderer.Camera (Mouse(..))

data Input = Input { moveX :: !Float, moveZ :: !Float, jump :: !Bool, restart :: !Bool, action :: !Bool }
  deriving (Eq,Show)
data Gamepad = Gamepad { connected :: !Bool, stickX :: !Float, stickY :: !Float, heldButtons :: !S.IntSet } deriving (Eq,Show)
neutralGamepad :: Gamepad
neutralGamepad=Gamepad False 0 0 S.empty
-- Radial dead zone retains analog magnitude and clamps overshooting devices.
radialStick :: Float -> Float -> (Float,Float)
radialStick x y | any (\v->isNaN v || isInfinite v) [x,y] = (0,0)
                | radius<=0.18 = (0,0)
                | otherwise = let factor=(min 1 radius-0.18)/(0.82*radius) in (x*factor,y*factor)
  where radius=sqrt (x*x+y*y)
data FrameInput = FrameInput
  { heldKeys :: !S.IntSet, pressedKeys :: !S.IntSet, pointer :: !Mouse
  , leftDown :: !Bool, leftReleased :: !Bool, gamepad :: !Gamepad }
emptyFrame :: FrameInput
emptyFrame=FrameInput S.empty S.empty (Mouse 0 0 0 0 0 False False False True) False False neutralGamepad
keyHeld, keyPressed :: Int -> FrameInput -> Bool
keyHeld k=S.member k . heldKeys
keyPressed k=S.member k . pressedKeys
gameInput :: Bool -> FrameInput -> Input
gameInput focused _ | focused=Input 0 0 False False False
gameInput _ frame=Input (clamp (axis [68,262] [65,263]+sx)) (clamp (axis [83,264] [87,265]+sy)) (held 32 || button 7) (held 82 || button 15) (held 69 || button 8)
  where pad=gamepad frame
        (sx,sy)=if connected pad then radialStick (stickX pad) (stickY pad) else (0,0)
        button key=connected pad && S.member key (heldButtons pad)
        clamp value=max (-1) (min 1 value)
        held k=keyHeld k frame
        axis pos neg=fromIntegral (fromEnum (any held pos)-fromEnum (any held neg))
inputValues :: Input -> [Float]
inputValues i=[moveX i,moveZ i,b (jump i),b (restart i),b (action i)]
  where b x=if x then 1 else 0
