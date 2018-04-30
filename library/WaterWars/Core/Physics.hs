module WaterWars.Core.Physics where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.PhysicsConstants
import WaterWars.Core.GameAction

-- TODO: module for the following
moveLocation :: VelocityVector -> Location -> Location
moveLocation (VelocityVector dx dy) (Location (x, y)) =
    Location (x + dx, y + dy)

jumpVector :: VelocityVector -> VelocityVector
jumpVector (VelocityVector x _) = VelocityVector x jumpForce

gravityVector :: VelocityVector
gravityVector = VelocityVector 0 (-gravityForce)

acceleratePlayer :: VelocityVector -> InGamePlayer -> InGamePlayer
acceleratePlayer v p@InGamePlayer {..} =
    p { playerVelocity = playerVelocity ++ v }

setPlayerVelocity :: VelocityVector -> InGamePlayer -> InGamePlayer
setPlayerVelocity v p = p { playerVelocity = v }

getBlock :: Location -> BlockLocation
getBlock (Location (x, y)) = BlockLocation (round x, round y)

runVelocityVector :: RunDirection -> VelocityVector -> VelocityVector
runVelocityVector RunLeft  (VelocityVector _ y) = VelocityVector (-runSpeed) y
runVelocityVector RunRight (VelocityVector _ y) = VelocityVector runSpeed y

velocityOnGround :: VelocityVector -> VelocityVector
velocityOnGround (VelocityVector x _) = VelocityVector x 0

blockLocationBelowFeet :: InGamePlayer -> BlockLocation
blockLocationBelowFeet InGamePlayer { playerLocation } =
    let Location (x, y) = playerLocation
    in  BlockLocation (round x, round $ y - 0.001)
