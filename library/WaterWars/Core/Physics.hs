module WaterWars.Core.Physics where

import           ClassyPrelude                     hiding ( Reader
                                                          , asks
                                                          )
import           WaterWars.Core.Game
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.Physics.Collision
import           WaterWars.Core.Physics.Utils
import           Control.Eff
import           Control.Eff.Reader.Strict


jumpVector :: VelocityVector -> VelocityVector
jumpVector (VelocityVector x _) = VelocityVector x jumpForce

-- TODO: onGround states
runVector :: Bool -> RunDirection -> VelocityVector
runVector True  RunLeft  = VelocityVector (-runAccelerationGround) 0
runVector True  RunRight = VelocityVector runAccelerationGround 0
runVector False RunLeft  = VelocityVector (-runAccelerationAir) 0
runVector False RunRight = VelocityVector runAccelerationAir 0

gravityVector :: VelocityVector
gravityVector = VelocityVector 0 (-gravityForce)

gravityPlayer :: Bool -> InGamePlayer -> InGamePlayer
gravityPlayer True  = id
gravityPlayer False = acceleratePlayer gravityVector

gravityProjectile :: Projectile -> Projectile
gravityProjectile = accelerateProjectile gravityVector

verticalDragPlayer :: Bool -> InGamePlayer -> InGamePlayer
verticalDragPlayer onGround player@InGamePlayer {..} =
    let VelocityVector vx vy = playerVelocity
        dragFactor = if onGround then verticalDragGround else verticalDragAir
    in  setPlayerVelocity (VelocityVector (vx * dragFactor) vy) player


isPlayerOnGround :: Member (Reader GameMap) e => InGamePlayer -> Eff e Bool
isPlayerOnGround player = do
    terrain <- asks gameTerrain
    let blocksBelowFeet = map blockBelow $ bottomPointsOfPlayer player
    return $ any (terrain `isSolidAt`) blocksBelowFeet

blockBelow :: Location -> BlockLocation
blockBelow (Location (x, y)) =
    BlockLocation (round x, round $ y - blockBelowTolerance)

movePlayer :: Member (Reader GameMap) e => InGamePlayer -> Eff e InGamePlayer
movePlayer player@InGamePlayer {..} = do
    terrain <- asks gameTerrain
    let playerCornerPoints = collisionPointsOfPlayer player
    let newStates = map
            (\p -> (p, moveWithCollision terrain p playerVelocity))
            playerCornerPoints
    let newState = leastOfStates playerLocation newStates
    return $ setPlayerMovementState newState player

-- TODO: test
leastMoveLocation :: Location -> [(Location, Location)] -> Location
leastMoveLocation startLocation =
    (`moveLocation` startLocation) . minimumVector . map (uncurry diffLocation)

-- TODO: test
leastOfStates :: Location -> [(Location, MovementState)] -> MovementState
leastOfStates startLocation successorStates =
    (leastMoveLocation startLocation locations, minimumVector velocities)
  where
    (locations, velocities) =
        unzip $ map (\(l, (l', v)) -> ((l, l'), v)) successorStates

-- TODO: test
getsHit :: InGamePlayer -> Projectile -> Bool
getsHit InGamePlayer {..} Projectile {..} =
    xRange `hasInside` px && yRange `hasInside` py
  where
    Location (px, py) = projectileLocation
    Location (x , y ) = playerLocation
    xRange            = (x - playerWidth / 2, x + playerWidth / 2)
    yRange            = (y, y + playerHeight)
