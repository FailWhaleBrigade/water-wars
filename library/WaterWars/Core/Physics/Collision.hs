module WaterWars.Core.Physics.Collision where

import           ClassyPrelude
import           WaterWars.Core.Game
import           WaterWars.Core.Physics.Geometry
import           Data.List                                ( nub )

-- TODO: some assertion that player cannot be inside a block
moveWithCollision
    :: Terrain -> Location -> VelocityVector -> MovementState
moveWithCollision terrain startLocation velocity =
    collisionMiddleware . fromMaybe noCollisionMovement $ do
        block <- collidingBlock terrain line
        collideWithBlock line block
  where
    noCollisionMovement = (moveLocation velocity startLocation, velocity)
    line                = Line startLocation velocity
    collisionMiddleware :: MovementState -> MovementState
    collisionMiddleware = id
    -- collisionMiddleware collisionResult = unsafePerformIO $ do
    --     -- putStrLn $ tshow (line, collisionResult)
    --     let Location (x, y)            = startLocation
    --     let VelocityVector vx vy       = velocity
    --     let (endLocation, endVelocity) = collisionResult
    --     let Location (x', y')          = endLocation
    --     let VelocityVector vx' vy'     = endVelocity
    --     let collisionIndicator =
    --             if collisionResult == noCollisionMovement then "N" else "C"
    --
    --     let approximateBlock = getApproximateBlock endLocation
    --     let insideSolidBlock = endLocation `isInsideBlock` approximateBlock && terrain `isSolidAt` approximateBlock
    --     let solidBlockIndicator = if insideSolidBlock then "!" else ""
    --
    --     putStrLn
    --         $  tshow x
    --         ++ ";"
    --         ++ tshow y
    --         ++ ";"
    --         ++ tshow vx
    --         ++ ";"
    --         ++ tshow vy
    --         ++ ";"
    --         ++ tshow x'
    --         ++ ";"
    --         ++ tshow y'
    --         ++ ";"
    --         ++ tshow vx'
    --         ++ ";"
    --         ++ tshow vy'
    --         ++ ";"
    --         ++ collisionIndicator ++ solidBlockIndicator
    --     return collisionResult

collideWithBlock :: Line -> BlockLocation -> Maybe MovementState
collideWithBlock line block =
    headMay . mapMaybe (collideWithBlockBorder line) $ getPossibleCollisionLines
        line
        block


collideWithBlockBorder :: Line -> Line -> Maybe MovementState
collideWithBlockBorder line borderLine@Line { lineVector = borderVector } = do
    i <- intersectionOfSegments line borderLine
    let collisionVelocity = lineVector line `truncateOnBorderLine` borderVector
    return (intersectionLocation i, collisionVelocity)


-- | get a list of candidate-blocks that the line may traverse
--
-- This collects a list of blocks that go in the general direction
getTraversedBlocksCandidates :: Line -> [BlockLocation]
getTraversedBlocksCandidates Line {..} = nub
    [ approximateBlock
    , BlockLocation (bx + xDirection, by)
    , BlockLocation (bx, by + yDirection)
    , BlockLocation (bx + xDirection, by + yDirection)
    ]
  where
    approximateBlock@(BlockLocation (bx, by)) =
        getApproximateBlock lineStartLocation
    VelocityVector vx vy = lineVector
    xDirection           = round $ signum vx
    yDirection           = round $ signum vy

-- | get the blocks that are actually traversed by the line - in the order of
-- traversal
getTraversedBlocks :: Line -> [BlockLocation]
getTraversedBlocks line =
    sortBy (compareTraversedBlocks line)
        . filter (line `traversesBlock`)
        $ getTraversedBlocksCandidates line

-- | compare blocks by ordering in which they are traversed by the line
compareTraversedBlocks :: Line -> BlockLocation -> BlockLocation -> Ordering
compareTraversedBlocks Line { lineStartLocation = Location (x, y) } =
    compare `on` absDistance
  where
    absDistance (BlockLocation (bx, by)) =
        abs (fromIntegral bx - x) + abs (fromIntegral by - y)

-- TODO: test more??
collidingBlock :: Terrain -> Line -> Maybe BlockLocation
collidingBlock terrain =
    headMay . filter (isSolidAt terrain) . getTraversedBlocks


-- get a list of possible block-borders a line can pass through
getPossibleCollisionLines :: Line -> BlockLocation -> [Line]
getPossibleCollisionLines line block = catMaybes
    [topLine, botLine, leftLine, rightLine]
  where
    Location (x, y) = lineStartLocation line
    leftX           = blockLeftX block
    rightX          = blockRightX block
    botY            = blockBotY block
    topY            = blockTopY block
    topLine         = if y >= topY then Just $ blockTopLine block else Nothing
    botLine         = if y <= botY then Just $ blockBotLine block else Nothing
    leftLine        = if x <= leftX then Just $ blockLeftLine block else Nothing
    rightLine = if x >= rightX then Just $ blockRightLine block else Nothing


-- truncate the velocity vector if it hit the border of a block
truncateOnBorderLine :: VelocityVector -> VelocityVector -> VelocityVector
truncateOnBorderLine velocityVector borderVector = VelocityVector
    (if by /= 0 then 0 else vx)
    (if bx /= 0 then 0 else vy)
  where
    VelocityVector vx vy = velocityVector
    VelocityVector bx by = borderVector
