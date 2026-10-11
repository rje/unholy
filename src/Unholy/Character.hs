-- Capsule locomotion and camera policy in Haskell; Bullet owns collision queries.
module Unholy.Character (CharacterSettings (..), defaultCharacter, driveCharacter, followCamera) where

import qualified Data.IntMap.Strict as IM
import Unholy.Input
import Unholy.Physics
import Unholy.Transform (setTransform)
import Unholy.Types

data CharacterSettings = CharacterSettings {walkSpeed :: !Float, jumpSpeed :: !Float, stepHeight :: !Float} deriving (Eq, Show)

defaultCharacter :: CharacterSettings
defaultCharacter = CharacterSettings 4.4 7 0.32

driveCharacter :: Physics -> CharacterSettings -> Float -> Bool -> Input -> Entity -> World -> IO (World, Bool, Float)
driveCharacter physics settings yaw leap controls entity w = case (IM.lookup entity (bodies w), IM.lookup entity (transforms w)) of
  (Just body, Just pose) -> do
    let size = max 1 (sqrt (moveX controls ^ (2 :: Int) + moveZ controls ^ (2 :: Int)))
        dx = (moveX controls * cos yaw + moveZ controls * sin yaw) / size
        dz = (moveZ controls * cos yaw - moveX controls * sin yaw) / size
        V3 _ vy _ = velocity body
        jumping = leap && grounded body
        moving = abs dx + abs dz > 0.001
        speed = walkSpeed settings
        nextBody = body {velocity = V3 (dx * speed) (if jumping then jumpSpeed settings else vy) (dz * speed)}
        facing = atan2 (-dx) (-dz) * 180 / pi
        V3 x y z = position pose
        V3 _ uniform _ = scale pose
        foot = y - uniform
        reach = uniform * 0.5 + 0.16
        toward height = V3 (x + dx * reach) height (z + dz * reach)
        query a b = raycastSolid physics w a b (Just entity)
    stepped <-
      if not (grounded body && moving && not jumping && stepHeight settings > 0)
        then pure Nothing
        else do
          low <- query (V3 x (foot + 0.10) z) (toward (foot + 0.10))
          high <- query (V3 x (foot + stepHeight settings + 0.08) z) (toward (foot + stepHeight settings + 0.08))
          landing <- query (toward (foot + stepHeight settings + 0.08)) (toward (foot + 0.01))
          pure $ case (low, high, landing) of
            (Just _, Nothing, Just hit) | let V3 _ normalY _ = hitNormal hit; V3 _ height _ = hitPoint hit, normalY > 0.65, height - foot > 0.03, height - foot <= stepHeight settings -> Just (height - foot + 0.02)
            _ -> Nothing
    case stepped of
      Just height ->
        requestTeleport physics entity
          >> pure (setTransform entity pose {position = V3 x (y + height) z} w {bodies = IM.insert entity nextBody (bodies w)}, jumping, facing)
      Nothing -> pure (w {bodies = IM.insert entity nextBody (bodies w)}, jumping, facing)
  _ -> pure (w, False, 0)

followCamera :: Physics -> World -> Entity -> Float -> Float -> IO Camera
followCamera physics w entity yaw pitch = do
  let V3 x y z = maybe (V3 0 0 0) position (IM.lookup entity (transforms w))
      focus = V3 x (y + 0.45) z
      distance = 6.4
      vertical = max 0.18 (min 1.15 pitch)
      offset = V3 (sin yaw * cos vertical * distance) (sin vertical * distance) (cos yaw * cos vertical * distance)
      destination = add focus offset
  hit <- raycastSolid physics w focus destination (Just entity)
  let fraction = maybe 1 (\h -> max 0.08 (hitFraction h - 0.06)) hit
  pure (Camera (add focus (multiply fraction offset)) focus 52)
  where
    add (V3 a b c) (V3 x y z) = V3 (a + x) (b + y) (c + z)
    multiply s (V3 x y z) = V3 (s * x) (s * y) (s * z)
