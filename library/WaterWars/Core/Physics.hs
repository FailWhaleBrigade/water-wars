module WaterWars.Core.Physics where

import           ClassyPrelude                     hiding ( Reader, asks )
import           WaterWars.Core.GameState
import           WaterWars.Core.GameMap
import           WaterWars.Core.GameUtils
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.GameAction
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Data.Array.IArray
import           WaterWars.Core.Terrain.Block


jumpVector :: VelocityVector -> VelocityVector
jumpVector (VelocityVector x _) = VelocityVector x jumpForce

runVector :: Bool -> RunDirection -> VelocityVector
runVector True  RunLeft  = VelocityVector (-runAccelerationGround) 0
runVector True  RunRight = VelocityVector runAccelerationGround 0
runVector False RunLeft  = VelocityVector (-runAccelerationAir) 0
runVector False RunRight = VelocityVector runAccelerationAir 0

gravityVector :: VelocityVector
gravityVector = VelocityVector 0 (-gravityForce)

velocityOnGround :: VelocityVector -> VelocityVector
velocityOnGround (VelocityVector x _) = VelocityVector x 0

blockLocationBelowFeet :: InGamePlayer -> BlockLocation
blockLocationBelowFeet InGamePlayer { playerLocation } =
    let Location (x, y) = playerLocation
    in  BlockLocation (round x, round $ y - 0.001)

gravityPlayer :: InGamePlayer -> InGamePlayer
gravityPlayer = acceleratePlayer gravityVector

-- TODO: better drag with polar coordinates
verticalDragPlayer :: Bool -> InGamePlayer -> InGamePlayer
verticalDragPlayer onGround player@InGamePlayer {..} =
    let VelocityVector vx vy = playerVelocity
        dragFactor = if onGround then verticalDragGround else verticalDragAir
    in  setPlayerVelocity (VelocityVector (vx * dragFactor) vy) player


isPlayerOnGround :: Member (Reader GameMap) e => InGamePlayer -> Eff e Bool
isPlayerOnGround InGamePlayer {..} = do
    blocks <- asks $ terrainBlocks . gameTerrain
    let Location (x, y) = playerLocation
    let blockBelowFeet  = BlockLocation (round x, round $ y - 0.001)
    return $ inRange (bounds blocks) blockBelowFeet && isSolid
        (blocks ! blockBelowFeet)

movePlayer :: Member (Reader GameMap) e => InGamePlayer -> Eff e InGamePlayer
movePlayer player@InGamePlayer {..} = do
    blocks <- asks $ terrainBlocks . gameTerrain
    let targetLocation = moveLocation playerVelocity playerLocation
    let targetBlock    = getBlock targetLocation
    let isTargetBlockSolid = inRange (bounds blocks) targetBlock
            && isSolid (blocks ! targetBlock)
    let realTargetLocation = if isTargetBlockSolid
            then
                let Location      (x, _) = targetLocation
                    BlockLocation (_, y) = targetBlock
                in  Location (x, fromIntegral y + 0.5)
            else targetLocation
    let realPlayerVelocity = if isTargetBlockSolid
            then velocityOnGround playerVelocity
            else playerVelocity
    return player { playerLocation = realTargetLocation
                  , playerVelocity = realPlayerVelocity
                  }
