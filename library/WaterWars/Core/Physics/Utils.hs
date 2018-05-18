module WaterWars.Core.Physics.Utils where

import           ClassyPrelude
import           WaterWars.Core.Game

angleFromVector :: VelocityVector -> Angle
angleFromVector (VelocityVector vx vy) = Angle $ atan2 vx vy

velocityBoundX :: Float -> VelocityVector -> VelocityVector
velocityBoundX maxX v@(VelocityVector vx vy) =
    if abs vx <= maxX then v else VelocityVector (boundedBy (-maxX, maxX) vx) vy

distanceFromLine' :: Location -> VelocityVector -> BlockLocation -> Float
distanceFromLine' (Location (x, y)) (VelocityVector vx vy) (BlockLocation (bx, by))
    = abs $ (x - fromIntegral bx) * nx + (y - fromIntegral by) * ny
    where
        nx = vy
        ny = -vx

-- TODO: test distance.

velocityOnCollisionY :: VelocityVector -> VelocityVector
velocityOnCollisionY (VelocityVector x _) = VelocityVector x 0
velocityOnCollisionX :: VelocityVector -> VelocityVector
velocityOnCollisionX (VelocityVector _ y) = VelocityVector 0 y

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
