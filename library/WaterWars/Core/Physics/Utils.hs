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

cornerPointsOfPlayer :: InGamePlayer -> [Location]
cornerPointsOfPlayer InGamePlayer {..} =
    let Location (x, y) = playerLocation
        leftBorder      = x - playerWidth / 2
        rightBorder     = x + playerWidth / 2
        bottomBorder    = y
        topBorder       = y + playerHeight
    in  [ Location (leftBorder, bottomBorder)
        , Location (rightBorder, bottomBorder)
        , Location (rightBorder, topBorder)
        , Location (leftBorder, topBorder)
        ]

bottomPointsOfPlayer :: InGamePlayer -> [Location]
bottomPointsOfPlayer InGamePlayer {..} =
    let Location (x, y) = playerLocation
        leftBorder      = x - playerWidth / 2
        rightBorder     = x + playerWidth / 2
    in  [ Location (leftBorder, y)
        , playerLocation
        , Location (rightBorder, y)
        ]

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


isInsideBlock :: Location -> BlockLocation -> Bool
isInsideBlock (Location (x, y)) block =
    blockRangeX block `hasInside` x && blockRangeY block `hasInside` y

hasInside :: Ord a => (a, a) -> a -> Bool
hasInside (l, u) x = l < x && x < u

containsInclusive :: Ord a => (a, a) -> a -> Bool
containsInclusive (l, u) x = l <= x && x <= u

boundedBy :: Ord a => (a, a) -> a -> a
boundedBy (l, u) x | x < l     = l
                   | x > u     = u
                   | otherwise = x
