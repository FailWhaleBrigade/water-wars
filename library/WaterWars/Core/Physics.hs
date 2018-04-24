module WaterWars.Core.Physics where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.PhysicsConstants

-- TODO: module for the following
moveLocation :: VelocityVector -> Location -> Location
moveLocation (VelocityVector dx dy) (Location (x, y)) =
    Location (x + dx, y + dy)

jumpVector :: VelocityVector
jumpVector = VelocityVector 0 jumpForce

gravityVector :: VelocityVector
gravityVector = VelocityVector 0 (-gravityForce)

acceleratePlayer :: VelocityVector -> InGamePlayer -> InGamePlayer
acceleratePlayer v p@InGamePlayer {..} =
    p { playerVelocity = playerVelocity ++ v }

getBlock :: Location -> BlockLocation
getBlock (Location (x, y)) = BlockLocation (round x, round y)
