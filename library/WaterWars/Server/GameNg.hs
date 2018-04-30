{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WaterWars.Server.GameNg (runGameTick, gameTick) where

import ClassyPrelude hiding (Reader, ask, asks) -- hide MTL functions reexported by prelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import WaterWars.Core.Physics
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Control.Eff
import Data.Array.IArray
import WaterWars.Core.Terrain.Block

runGameTick :: GameMap -> GameState -> Map Player Action -> GameState
runGameTick gameMap gameState gameAction =
    run
        . execState gameState
        . runReader gameMap
        . runReader gameAction
        $ gameTick

gameTick
    :: ( Member (State GameState) e
       , Member (Reader (Map Player Action)) e
       , Member (Reader GameMap) e
       )
    => Eff e ()
gameTick = do
    mapMOverPlayers modifyPlayerByAction
    moveProjectiles
    mapMOverPlayers modifyPlayerByEnvironment
    mapMOverPlayers movePlayer
    return ()

-- | Moves all projectiles in the game. This is effectful since the movement
--   depends on the whole state
moveProjectiles :: (Member (State GameState) e) => Eff e ()
moveProjectiles = do
    Projectiles projectiles <- gets gameProjectiles
    let newProjectiles = map moveProjectile projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }

moveProjectile :: Projectile -> Projectile
moveProjectile (projectile@Projectile {..}) = projectile
    { projectileLocation = moveLocation projectileVelocity projectileLocation
    }

-- move player according to its velociy, but also bound it.
movePlayer :: Member (Reader GameMap) e => InGamePlayer -> Eff e InGamePlayer
movePlayer player@InGamePlayer {..} = do
    -- blocks <- asks $ terrainBlocks . gameTerrain
    let targetLocation     = moveLocation playerVelocity playerLocation
    -- let targetBlock        = getBlock targetLocation
    -- let isTargetBlockSolid = isSolid $ blocks ! targetBlock
    -- let realTargetLocation = if isTargetBlockSolid
    --         then
    --             let Location      (_, y) = targetLocation
    --                 BlockLocation (x, _) = targetBlock
    --             in  Location (fromIntegral x + 0.5, y)
    --         else targetLocation
    -- return player { playerLocation = realTargetLocation }
    return $ player { playerLocation = realTargetLocation }

-- | Function that includes the actions into a player-state
modifyPlayerByAction
    :: Member (Reader (Map Player Action)) e
    => InGamePlayer
    -> Eff e InGamePlayer
modifyPlayerByAction player = do
    actionMap :: Map Player Action <- ask
    let action =
            fromMaybe noAction $ lookup (playerDescription player) actionMap
    return
        . modifyPlayerByRunAction action
        . modifyPlayerByJumpAction action
        $ player

modifyPlayerByJumpAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByJumpAction action player = fromMaybe player $ do -- maybe monad
    JumpAction <- jumpAction action
    return $ acceleratePlayer jumpVector player

modifyPlayerByRunAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByRunAction action player = fromMaybe player $ do -- maybe monad
    RunAction runDirection <- runAction action
    return $ acceleratePlayer (runVelocityVector runDirection) player

-- do gravity, bounding, ...
modifyPlayerByEnvironment :: InGamePlayer -> Eff r InGamePlayer
modifyPlayerByEnvironment =
    return . truncatePlayerVelocity . gravityPlayer

gravityPlayer :: InGamePlayer -> InGamePlayer
gravityPlayer = acceleratePlayer gravityVector

truncatePlayerVelocity :: InGamePlayer -> InGamePlayer
truncatePlayerVelocity player@InGamePlayer {..} =
    player { playerVelocity = boundVelocityVector playerVelocity }

-- bound velocity vector to be max 0.5 in both directions
boundVelocityVector :: VelocityVector -> VelocityVector
boundVelocityVector v@(VelocityVector vx vy) = if abs vx < 0.5 && abs vy < 0.5
    then v
    else VelocityVector (boundedBy (-0.5, 0.5) vx) (boundedBy (-0.5, 0.5) vy)

boundedBy :: Ord a => (a, a) -> a -> a
boundedBy (l, u) x | x < l     = l
                   | x > u     = u
                   | otherwise = x

-- CUSTOM UTILITY FUNCTIONS

mapMOverPlayers
    :: (Member (State GameState) e, Member (Reader GameMap) e)
    => (InGamePlayer -> Eff e InGamePlayer)
    -> Eff e ()
mapMOverPlayers mapping = do
    InGamePlayers players <- gets inGamePlayers
    newPlayers            <- mapM mapping players
    modify $ \s -> s { inGamePlayers = InGamePlayers newPlayers }


-- GENERAL UTILITY FUNCTIONS

asks :: Member (Reader s) r => (s -> a) -> Eff r a
asks f = map f ask
{-# INLINE asks #-}

gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}
-- TODO: implement with lenses??
