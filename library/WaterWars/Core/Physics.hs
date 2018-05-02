module WaterWars.Core.Physics where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.GameMap
import WaterWars.Core.PhysicsConstants
import WaterWars.Core.GameAction

-- TODO: module for the following
moveLocation :: VelocityVector -> Location -> Location
moveLocation (VelocityVector dx dy) (Location (x, y)) =
    Location (x + dx, y + dy)

jumpVector :: VelocityVector -> VelocityVector
jumpVector (VelocityVector x _) = VelocityVector x jumpForce

runVector :: Bool -> RunDirection -> VelocityVector
runVector True RunLeft = VelocityVector (-runAccelerationGround) 0
runVector True RunRight = VelocityVector runAccelerationGround 0
runVector False RunLeft = VelocityVector (-runAccelerationAir) 0
runVector False RunRight = VelocityVector runAccelerationAir 0

gravityVector :: VelocityVector
gravityVector = VelocityVector 0 (-gravityForce)

acceleratePlayer :: VelocityVector -> InGamePlayer -> InGamePlayer
acceleratePlayer v p@InGamePlayer {..} =
    setPlayerVelocity (playerVelocity ++ v) p

setPlayerVelocity :: VelocityVector -> InGamePlayer -> InGamePlayer
setPlayerVelocity v p = p { playerVelocity = v }

modifyPlayerVelocity :: (VelocityVector -> VelocityVector) -> InGamePlayer -> InGamePlayer
modifyPlayerVelocity f p = setPlayerVelocity (f $ playerVelocity p) p

getBlock :: Location -> BlockLocation
getBlock (Location (x, y)) = BlockLocation (round x, round y)

velocityOnGround :: VelocityVector -> VelocityVector
velocityOnGround (VelocityVector x _) = VelocityVector x 0

blockLocationBelowFeet :: InGamePlayer -> BlockLocation
blockLocationBelowFeet InGamePlayer { playerLocation } =
    let Location (x, y) = playerLocation
    in  BlockLocation (round x, round $ y - 0.001)

velocityBoundX :: Float -> VelocityVector -> VelocityVector
velocityBoundX maxX v@(VelocityVector vx vy) = if abs vx <= maxX
    then v
    else VelocityVector (boundedBy (-maxX, maxX) vx) vy

-- bound velocity vector to be max 0.5 in both directions
boundVelocityVector :: (Float, Float) -> VelocityVector -> VelocityVector
boundVelocityVector (maxX, maxY) v@(VelocityVector vx vy) = if abs vx <= maxX && abs vy <= maxY
    then v
    else VelocityVector (boundedBy (-maxX, maxX) vx) (boundedBy (-maxY, maxY) vy)

boundedBy :: Ord a => (a, a) -> a -> a
boundedBy (l, u) x | x < l     = l
                   | x > u     = u
                   | otherwise = x
