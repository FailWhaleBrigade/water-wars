module WaterWars.Core.Physics.Utils where

import           ClassyPrelude
import           WaterWars.Core.GameState

angleFromVector :: VelocityVector -> Angle
angleFromVector (VelocityVector vx vy) = Angle $ atan2 vx vy

velocityBoundX :: Float -> VelocityVector -> VelocityVector
velocityBoundX maxX v@(VelocityVector vx vy) =
    if abs vx <= maxX then v else VelocityVector (boundedBy (-maxX, maxX) vx) vy

-- bound velocity vector to be max 0.5 in both directions
boundVelocityVector :: (Float, Float) -> VelocityVector -> VelocityVector
boundVelocityVector (maxX, maxY) v@(VelocityVector vx vy) =
    if abs vx <= maxX && abs vy <= maxY
        then v
        else VelocityVector (boundedBy (-maxX, maxX) vx)
                            (boundedBy (-maxY, maxY) vy)

boundedBy :: Ord a => (a, a) -> a -> a
boundedBy (l, u) x | x < l     = l
                   | x > u     = u
                   | otherwise = x
