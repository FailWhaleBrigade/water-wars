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
    if isSolidAt terrain (getBlock newLocation)
        then error $ "player enters block " ++ show
            (startLocation, velocity, newLocation, getBlock newLocation)
        else (newLocation, newVelocity)
  where
    (newLocation, newVelocity) =
        case collidingBlock terrain startLocation velocity of
            Nothing -> (moveLocation velocity startLocation, velocity)
            Just b  -> collideWithBlock startLocation velocity b
-- TODO: ensure that resulting location is not inside a block

collidingBlock :: Terrain -> Location -> VelocityVector -> Maybe BlockLocation
collidingBlock _ _ (VelocityVector 0 0) = Nothing
collidingBlock terrain startLocation velocity =
    case (middleBlockSolid, endBlockSolid) of
        (True, _   ) -> middleBlock -- garatueed to be Just
        (_   , True) -> Just endBlock
        _            -> Nothing
  where
    startBlock    = getBlock startLocation
    endLocation   = moveLocation velocity startLocation
    endBlock      = getBlock endLocation
    endBlockSolid = isSolidAt terrain endBlock
    middleBlock   = if isBlockDiagonal startBlock endBlock
        then
            minimumByMay (compare `on` distanceFromLine' startLocation velocity)
                $ tupleToList $ getNextToDiagonal startBlock endBlock
        else Nothing
    middleBlockSolid = case middleBlock of
        Just b  -> isSolidAt terrain b
        Nothing -> False
    tupleToList (a,b) = [a,b] -- TODO: refactor

collideWithBlock
    :: Location -> VelocityVector -> BlockLocation -> (Location, VelocityVector)
collideWithBlock startLocation@(Location (x, y)) velocity collideBlock =
    fromLeft
            -- (startLocation, velocity)
            (error $ "error in implementation of collision " ++ show
                (startLocation, velocity, collideBlock)
            )
        $ do
              let leftX  = blockLeftX collideBlock
              let rightX = blockRightX collideBlock
              let botY   = blockBotY collideBlock
              let topY   = blockTopY collideBlock
              when (x <= leftX)
                  . whenJust
                        (cutBlockBorderX startLocation
                                         velocity
                                         collideBlock
                                         leftX
                        )
                  $ \loc -> Left (loc, velocityOnCollisionX velocity)
              when (x >= rightX)
                  . whenJust
                        (cutBlockBorderX startLocation
                                         velocity
                                         collideBlock
                                         rightX
                        )
                  $ \loc -> Left (loc, velocityOnCollisionX velocity)
              when (y <= botY)
                  . whenJust
                        (cutBlockBorderY startLocation
                                         velocity
                                         collideBlock
                                         (botY - 0.002)
                        )
                  $ \loc -> Left (loc, velocityOnCollisionY velocity)
              when (y >= topY)
                  . whenJust
                        (cutBlockBorderY startLocation
                                         velocity
                                         collideBlock
                                         topY
                        )
                  $ \loc -> Left (loc, velocityOnCollisionY velocity)

cutBlockBorderX
    :: Location -> VelocityVector -> BlockLocation -> Float -> Maybe Location
cutBlockBorderX (Location (x, y)) (VelocityVector vx vy) block bx = do
    unless (t <= 1)                 Nothing
    unless (inBlockRangeY block by) Nothing
    Just $ Location (bx - corr, by)
  where -- solve (x y) + t (vx vy) = (bx by)
    t    = (bx - x) / vx
    by   = y + t * vy
    corr = 0.01 * signum vx -- correction term to avoid sticking to walls

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
