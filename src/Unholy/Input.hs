module Unholy.Input (Input (..), FrameInput (..), Gamepad (..), neutralGamepad, radialStick, emptyFrame, gameInput, inputValues, keyHeld, keyPressed, Binding (..), ActionFrame (..), defaultBindings, actionFrame, actionPressed, emptyActions) where

import qualified Data.IntSet as S
import qualified Data.Set as Set
import Unholy.Renderer.Camera (Mouse (..))

data Input = Input {moveX :: !Float, moveZ :: !Float, jump :: !Bool, restart :: !Bool, action :: !Bool}
  deriving (Eq, Show)

data Gamepad = Gamepad {connected :: !Bool, stickX :: !Float, stickY :: !Float, heldButtons :: !S.IntSet, lookStickX :: !Float, lookStickY :: !Float} deriving (Eq, Show)

neutralGamepad :: Gamepad
neutralGamepad = Gamepad False 0 0 S.empty 0 0

-- Radial dead zone retains analog magnitude and clamps overshooting devices.
radialStick :: Float -> Float -> (Float, Float)
radialStick x y
  | any (\v -> isNaN v || isInfinite v) [x, y] = (0, 0)
  | radius <= 0.18 = (0, 0)
  | otherwise = let factor = (min 1 radius - 0.18) / (0.82 * radius) in (x * factor, y * factor)
  where
    radius = sqrt (x * x + y * y)

data FrameInput = FrameInput
  { heldKeys :: !S.IntSet,
    pressedKeys :: !S.IntSet,
    pointer :: !Mouse,
    leftDown :: !Bool,
    leftReleased :: !Bool,
    gamepad :: !Gamepad
  }

emptyFrame :: FrameInput
emptyFrame = FrameInput S.empty S.empty (Mouse 0 0 0 0 0 False False False True) False False neutralGamepad

keyHeld, keyPressed :: Int -> FrameInput -> Bool
keyHeld k = S.member k . heldKeys
keyPressed k = S.member k . pressedKeys

gameInput :: Bool -> FrameInput -> Input
gameInput focused _ | focused = Input 0 0 False False False
gameInput _ frame = Input (clamp (axis [68, 262] [65, 263] + sx)) (clamp (axis [83, 264] [87, 265] + sy)) (held 32 || button 7) (held 82 || button 15) (held 69 || button 8)
  where
    pad = gamepad frame
    (sx, sy) = if connected pad then radialStick (stickX pad) (stickY pad) else (0, 0)
    button key = connected pad && S.member key (heldButtons pad)
    clamp value = max (-1) (min 1 value)
    held k = keyHeld k frame
    axis pos neg = fromIntegral (fromEnum (any held pos) - fromEnum (any held neg))

inputValues :: Input -> [Float]
inputValues i = [moveX i, moveZ i, b (jump i), b (restart i), b (action i)]
  where
    b x = if x then 1 else 0

-- Bindings are game-readable data; callers can replace or extend the defaults.
data Binding = Binding String [Int] [Int] deriving (Eq, Show)

data ActionFrame = ActionFrame
  { heldActions :: !(Set.Set String),
    actionPresseds :: !(Set.Set String),
    releasedActions :: !(Set.Set String),
    lookDelta :: !(Float, Float),
    activatedButtons :: [String]
  }
  deriving (Eq, Show)

emptyActions :: ActionFrame
emptyActions = ActionFrame Set.empty Set.empty Set.empty (0, 0) []

actionPressed :: String -> ActionFrame -> Bool
actionPressed key = Set.member key . actionPresseds

defaultBindings :: [Binding]
defaultBindings =
  [ Binding "jump" [32] [7],
    Binding "interact" [69] [8],
    Binding "pause" [256] [15],
    Binding "confirm" [257, 32] [7],
    Binding "back" [256] [6],
    Binding "up" [265, 87] [1],
    Binding "down" [264, 83] [3]
  ]

actionFrame :: [Binding] -> Float -> FrameInput -> FrameInput -> ActionFrame
actionFrame bindings dt previous current = ActionFrame held pressed (Set.difference before held) look []
  where
    active frame (Binding _ keys buttons) = any (`keyHeld` frame) keys || (connected (gamepad frame) && any (`S.member` heldButtons (gamepad frame)) buttons)
    held = Set.fromList [name | b@(Binding name _ _) <- bindings, active current b]
    before = Set.fromList [name | b@(Binding name _ _) <- bindings, active previous b]
    pressed = Set.union (Set.difference held before) (Set.fromList [name | Binding name keys _ <- bindings, any (`keyPressed` current) keys])
    mouse = pointer current
    (sx, sy) = if connected (gamepad current) then radialStick (lookStickX (gamepad current)) (lookStickY (gamepad current)) else (0, 0)
    look =
      ( if rightDown mouse then deltaX mouse * 0.004 + sx * dt * 2 else sx * dt * 2,
        if rightDown mouse then deltaY mouse * 0.004 + sy * dt * 2 else sy * dt * 2
      )
