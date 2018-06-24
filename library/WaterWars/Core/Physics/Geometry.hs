module WaterWars.Core.Physics.Geometry where

import           ClassyPrelude
import           WaterWars.Core.Game
import           WaterWars.Core.Physics.Utils

data Line = Line
    { lineStartLocation :: Location
    , lineVector :: VelocityVector
    }
    deriving (Show, Read, Eq)

data Intersection = Intersection
    { intersectionLocation :: Location
    , intersectionVector1Scale :: Float
    , intersectionVector2Scale :: Float
    }
    deriving (Show, Read, Eq)

lineEndPoint :: Line -> Location
lineEndPoint Line {..} = moveLocation lineVector lineStartLocation

normalVector :: VelocityVector -> VelocityVector
normalVector VelocityVector {..} = VelocityVector (-velocityY) velocityX

normalLineThrough :: Line -> Location -> Line
normalLineThrough Line { lineVector } location =
    Line location $ normalVector lineVector

-- check if a line goes through the inside of a block.
-- the starting block is not considered as if the point ends in another block
traversesBlock :: Line -> BlockLocation -> Bool
traversesBlock line block =
    endPointIsInBlock || (passesThroughBlock && not startPointIsInBlock)
  where
    endPointIsInBlock   = lineEndPoint line `isInsideBlock` block
    startPointIsInBlock = lineStartLocation line `isInsideBlock` block
    passesThroughBlock  = isJust $ do
        Intersection nearestPoint t _ <- normalIntersection line
            $ blockLocationToLocation block
        unless (nearestPoint `isInsideBlock` block) Nothing
        unless ((0, 1) `hasInside` t)               Nothing

-- calculate the nearest point to a given location that lies on the line
normalIntersection :: Line -> Location -> Maybe Intersection
normalIntersection line location =
    intersectionOfLines line (line `normalLineThrough` location)

-- intersect 2 line segments reaching only from start-point to end-point.
intersectionOfSegments :: Line -> Line -> Maybe Intersection
intersectionOfSegments line1 line2 = do
    i@Intersection{..} <- intersectionOfLines line1 line2
    unless ((0, 1) `containsInclusive` intersectionVector1Scale) Nothing
    unless ((0, 1) `containsInclusive` intersectionVector2Scale) Nothing
    return i

-- intersect 2 lines
intersectionOfLines :: Line -> Line -> Maybe Intersection
intersectionOfLines line1 line2 = if abs determinant < 1e-3
    then Nothing
    else Just Intersection {..}
  where
    Line { lineStartLocation = Location (p1x, p1y), lineVector = VelocityVector v1x v1y }
        = line1
    Line { lineStartLocation = Location (p2x, p2y), lineVector = VelocityVector v2x v2y }
        = line2
    -- solve p1 + t1 v1 = p2 + t2 v2
    -- as linear system
    -- (v1x -v2x | p2x - p1x)
    -- (v1y -v2y | p2x - p1x)
    -- Matrix has determinant |A| = v1y v2x - v1x v2y
    -- A^(-1) = 1/d
    -- (-v2y v2x)
    -- (-v1y v1x)
    --
    -- solution
    -- t = A^(-1)*p
    --
    -- (t1) = (-v2y v2x) (px)
    -- (t2) = (-v1y v1x) (py)
    --
    -- t1 = (-v2y*px + v2x*py)/d
    -- t2 = (-v1y*px + v1x*py)/d
    determinant              = v1y * v2x - v1x * v2y
    px                       = p2x - p1x
    py                       = p2y - p1y
    t1                       = (-v2y * px + v2x * py) / determinant
    t2                       = (-v1y * px + v1x * py) / determinant

    -- calculate intersection using average of 2 thoratically correct results
    -- i1 = p1 + t1 v1
    -- i2 = p2 + t2 v2
    -- (theoretically: i1 == i2)
    i1x                      = p1x + t1 * v1x
    i1y                      = p1y + t1 * v1y
    i2x                      = p2x + t2 * v2x
    i2y                      = p2y + t2 * v2y

    intersectionLocation     = Location ((i1x + i2x) / 2, (i1y + i2y) / 2)
    intersectionVector1Scale = t1
    intersectionVector2Scale = t2

-- TODO: test??
blockTopLine :: BlockLocation -> Line
blockTopLine block =
    Line (Location (blockLeftX block, blockTopY block)) (VelocityVector 1 0)

blockBotLine :: BlockLocation -> Line
blockBotLine block =
    Line (Location (blockLeftX block, blockBotY block)) (VelocityVector 1 0)

blockRightLine :: BlockLocation -> Line
blockRightLine block =
    Line (Location (blockRightX block, blockBotY block)) (VelocityVector 0 1)

blockLeftLine :: BlockLocation -> Line
blockLeftLine block =
    Line (Location (blockLeftX block, blockBotY block)) (VelocityVector 0 1)
