module WaterWars.Core.Physics.Collision where

import           ClassyPrelude
import           WaterWars.Core.GameState
import           WaterWars.Core.GameMap
import           WaterWars.Core.GameUtils
import           WaterWars.Core.Physics.Utils
import           Data.Either.Combinators                  ( fromLeft )
import           Control.Monad.Extra                      ( whenJust )

moveWithCollision
    :: Terrain -> Location -> VelocityVector -> (Location, VelocityVector)
moveWithCollision terrain startLocation velocity =
    case collidingBlock terrain startLocation velocity of
        Nothing -> (moveLocation velocity startLocation, velocity)
        Just b  -> collideWithBlock startLocation velocity b

collidingBlock
    :: Terrain -> Location -> VelocityVector -> Maybe BlockLocation
collidingBlock terrain startLocation velocity =
    case (middleBlockSolid, endBlockSolid) of
        (True, _   ) -> middleBlock -- garatueed to be Just
        (_   , True) -> Just endBlock
        _            -> Nothing
  where
    mapBounds     = terrainBounds terrain
    startBlock    = getBlock startLocation
    endLocation   = moveLocation velocity startLocation
    endBlock      = getBlock endLocation
    endBlockSolid = isSolidAt terrain endBlock
    middleBlock   = if isBlockDiagonal startBlock endBlock
        then
            minimumByMay (compare `on` distanceFromLine' startLocation velocity)
                $ get4NeighborBlocks mapBounds startBlock
        else Nothing
    middleBlockSolid = case middleBlock of
        Just b  -> isSolidAt terrain b
        Nothing -> False

collideWithBlock
    :: Location -> VelocityVector -> BlockLocation -> (Location, VelocityVector)
collideWithBlock startLocation@(Location (x, y)) velocity collideBlock =
    fromLeft (error "error in implementation of collision") $ do
        let leftX  = blockLeftX collideBlock
        let rightX = blockRightX collideBlock
        let botY   = blockBotY collideBlock
        let topY   = blockTopY collideBlock
        when (x <= leftX)
            . whenJust
                  (cutBlockBorderX startLocation velocity collideBlock leftX)
            $ \loc -> Left (loc, velocityOnCollisionX velocity)
        when (x >= rightX)
            . whenJust
                  (cutBlockBorderX startLocation velocity collideBlock rightX)
            $ \loc -> Left (loc, velocityOnCollisionX velocity)
        when (y <= botY)
            . whenJust
                  (cutBlockBorderY startLocation velocity collideBlock botY)
            $ \loc -> Left (loc, velocityOnCollisionY velocity)
        when (y >= topY)
            . whenJust
                  (cutBlockBorderY startLocation velocity collideBlock topY)
            $ \loc -> Left (loc, velocityOnCollisionY velocity)

cutBlockBorderX
    :: Location -> VelocityVector -> BlockLocation -> Float -> Maybe Location
cutBlockBorderX (Location (x, y)) (VelocityVector vx vy) block bx = do
    unless (t <= 1)                 Nothing
    unless (inBlockRangeY block by) Nothing
    Just $ Location (bx, by)
  where
    t  = (bx - x) / vx
    by = y + t * vy

cutBlockBorderY
    :: Location -> VelocityVector -> BlockLocation -> Float -> Maybe Location
cutBlockBorderY (Location (x, y)) (VelocityVector vx vy) block by = do
    unless (t <= 1)                 Nothing
    unless (inBlockRangeX block bx) Nothing
    Just $ Location (bx, by)
  where
    t  = (by - y) / vy
    bx = x + t * vx

isBlockDiagonal :: BlockLocation -> BlockLocation -> Bool
isBlockDiagonal b1 b2 = manhattanDistance b1 b2 == 2
