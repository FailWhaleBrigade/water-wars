module WaterWars.Core.Game.Utils
    ( module WaterWars.Core.Game.Utils
    , module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Map
    , module WaterWars.Core.Game.Action
    )
where

import           ClassyPrelude                     hiding ( Reader
                                                          , ask
                                                          )
import           WaterWars.Core.Game.State
import           WaterWars.Core.Game.Map
import           WaterWars.Core.Game.Action
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           WaterWars.Core.Physics.Constants
import           Data.Array.IArray

-- TODO: refactor?

moveLocation :: VelocityVector -> Location -> Location
moveLocation (VelocityVector dx dy) (Location (x, y)) =
    Location (x + dx, y + dy)

setPlayerCooldown :: InGamePlayer -> InGamePlayer
setPlayerCooldown player = player { playerShootCooldown = shootCooldown }

acceleratePlayer :: VelocityVector -> InGamePlayer -> InGamePlayer
acceleratePlayer v p@InGamePlayer {..} =
    setPlayerVelocity (playerVelocity ++ v) p

setPlayerVelocity :: VelocityVector -> InGamePlayer -> InGamePlayer
setPlayerVelocity v p = p { playerVelocity = v }

modifyPlayerVelocity
    :: (VelocityVector -> VelocityVector) -> InGamePlayer -> InGamePlayer
modifyPlayerVelocity f p = setPlayerVelocity (f $ playerVelocity p) p

getBlock :: Location -> BlockLocation
getBlock (Location (x, y)) = BlockLocation (round x, round $ y + 0.001)

moveProjectile :: Projectile -> Projectile
moveProjectile (projectile@Projectile {..}) = projectile
    { projectileLocation = moveLocation projectileVelocity projectileLocation
    }

accelerateProjectile :: VelocityVector -> Projectile -> Projectile
accelerateProjectile v p@Projectile {..} =
    setProjectileVelocity (projectileVelocity ++ v) p

setProjectileVelocity :: VelocityVector -> Projectile -> Projectile
setProjectileVelocity v p = p { projectileVelocity = v }

modifyProjectileVelocity
    :: (VelocityVector -> VelocityVector) -> Projectile -> Projectile
modifyProjectileVelocity f p =
    setProjectileVelocity (f $ projectileVelocity p) p

newProjectileFromAngle :: Location -> Angle -> Projectile
newProjectileFromAngle loc angle =
    Projectile loc $ velocityVectorFromPolar projectileSpeed angle

velocityVectorFromPolar :: Speed -> Angle -> VelocityVector
velocityVectorFromPolar (Speed speed) (Angle angle) =
    VelocityVector (speed * cos angle) (speed * sin angle)

addProjectile :: Member (State GameState) e => Projectile -> Eff e ()
addProjectile projectile = do
    Projectiles projectiles <- gets gameProjectiles
    let newProjectiles = projectile `cons` projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }

angleForRunDirection :: RunDirection -> Angle
angleForRunDirection RunRight = Angle 0
angleForRunDirection RunLeft  = Angle pi

get4NeighborBlocks
    :: (BlockLocation, BlockLocation) -> BlockLocation -> [BlockLocation]
get4NeighborBlocks mapBounds (BlockLocation (x, y)) = filter
    (inRange mapBounds)
    [ BlockLocation (x + 1, y)
    , BlockLocation (x, y + 1)
    , BlockLocation (x - 1, y)
    , BlockLocation (x, y - 1)
    ]

-- TODO: comment
getNextToDiagonal
    :: BlockLocation -> BlockLocation -> (BlockLocation, BlockLocation)
getNextToDiagonal (BlockLocation (b1x, b1y)) (BlockLocation (b2x, b2y)) =
    (BlockLocation (b1x, b2y), BlockLocation (b2x, b1y))

isSolidAt :: Terrain -> BlockLocation -> Bool
isSolidAt terrain location = inRange (terrainBounds terrain) location
    && isSolid (terrain `blockAt` location)

manhattanDistance :: BlockLocation -> BlockLocation -> Int
manhattanDistance (BlockLocation (x1, y1)) (BlockLocation (x2, y2)) =
    abs (x2 - x1) + abs (y2 - y1)

blockLeftX :: BlockLocation -> Float
blockLeftX (BlockLocation (x, _)) = fromIntegral x - 0.5
blockRightX :: BlockLocation -> Float
blockRightX (BlockLocation (x, _)) = fromIntegral x + 0.5
blockBotY :: BlockLocation -> Float
blockBotY (BlockLocation (_, y)) = fromIntegral y - 0.5
blockTopY :: BlockLocation -> Float
blockTopY (BlockLocation (_, y)) = fromIntegral y + 0.5
blockRangeX :: BlockLocation -> (Float, Float)
blockRangeX b = (blockLeftX b, blockRightX b)
blockRangeY :: BlockLocation -> (Float, Float)
blockRangeY b = (blockBotY b, blockTopY b)
inBlockRangeX :: BlockLocation -> Float -> Bool
inBlockRangeX b bx = blockLeftX b <= bx && bx <= blockRightX b
inBlockRangeY :: BlockLocation -> Float -> Bool
inBlockRangeY b by = blockBotY b <= by && by <= blockTopY b

asks :: Member (Reader s) r => (s -> a) -> Eff r a
asks f = map f ask
{-# INLINE asks #-}

gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}


mapMOverPlayers
    :: (Member (State GameState) e, Member (Reader GameMap) e)
    => (InGamePlayer -> Eff e InGamePlayer)
    -> Eff e ()
mapMOverPlayers mapping = do
    InGamePlayers players <- gets inGamePlayers
    newPlayers            <- mapM mapping players
    modify $ \s -> s { inGamePlayers = InGamePlayers newPlayers }

mapMOverProjectiles
    :: (Member (State GameState) e, Member (Reader GameMap) e)
    => (Projectile -> Eff e Projectile)
    -> Eff e ()
mapMOverProjectiles mapping = do
    Projectiles projectiles <- gets gameProjectiles
    newProjectiles          <- mapM mapping projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }

filterMOverProjectiles
    :: (Member (State GameState) e, Member (Reader GameMap) e)
    => (Projectile -> Eff e Bool)
    -> Eff e ()
filterMOverProjectiles predicate = do
    Projectiles projectiles <- gets gameProjectiles
    newProjectiles          <- filterM predicate projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }
