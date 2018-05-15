module WaterWars.Core.GameUtils where

import           ClassyPrelude                     hiding ( Reader
                                                          , ask
                                                          )
import           WaterWars.Core.GameState
import           WaterWars.Core.GameMap
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.GameAction

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
getBlock (Location (x, y)) = BlockLocation (round x, round y)

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
modifyProjectileVelocity f p = setProjectileVelocity (f $ projectileVelocity p) p

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
