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
import WaterWars.Core.PhysicsConstants
import Data.Array.IArray
import WaterWars.Core.Terrain.Block

runGameTick :: GameMap -> GameState -> Map Player Action -> GameState
runGameTick gameMap gameState gameAction =
    run
        . flip execState gameState
        . flip runReader gameMap
        . flip runReader gameAction
        $ gameTick

gameTick
    :: ( Member (State GameState) e
       , Member (Reader (Map Player Action)) e
       , Member (Reader GameMap) e
       )
    => Eff e ()
gameTick = do
    applyActionsToPlayers
    moveProjectiles
    movePlayers
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

movePlayers
    :: (Member (State GameState) e, Member (Reader GameMap) e) => Eff e ()
movePlayers = do
    InGamePlayers players <- gets inGamePlayers
    newPlayers            <- mapM movePlayer players
    modify $ \s -> s { inGamePlayers = InGamePlayers newPlayers }

movePlayer :: Member (Reader GameMap) e => InGamePlayer -> Eff e InGamePlayer
movePlayer player@InGamePlayer {..} = do
    blocks <- asks $ terrainBlocks . gameTerrain
    let targetLocation     = moveLocation playerVelocity playerLocation
    let targetBlock        = getBlock targetLocation
    let isTargetBlockSolid = isSolid $ blocks ! targetBlock
    let realTargetLocation = if isTargetBlockSolid
            then
                let Location      (_, y) = targetLocation
                    BlockLocation (x, _) = targetBlock
                in  Location (fromIntegral x + 0.5, y)
            else targetLocation
    return player { playerLocation = realTargetLocation }

-- | Applies the actions given for each player to the player-obects
applyActionsToPlayers
    :: (Member (State GameState) e, Member (Reader (Map Player Action)) e)
    => Eff e ()
applyActionsToPlayers = do
    perPlayer <- actionsPerPlayer
    let modifiedPlayers = map (uncurry modifyPlayerByAction) perPlayer
    modify $ \s -> s { inGamePlayers = InGamePlayers modifiedPlayers }

-- | Function that includes the actions into a player-state
-- TODO improve action type & implementation of this function
modifyPlayerByAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByAction action =
    modifyPlayerByRunAction action . modifyPlayerByJumpAction action

modifyPlayerByJumpAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByJumpAction action player = fromMaybe player $ do -- maybe monad
    JumpAction <- jumpAction action
    return $ acceleratePlayer jumpVector player

modifyPlayerByRunAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByRunAction action player = fromMaybe player $ do -- maybe monad
    RunAction runDirection <- runAction action
    return $ acceleratePlayer (runVelocityVector runDirection) player

-- get action / player tuples
actionsPerPlayer
    :: (Member (State GameState) e, Member (Reader (Map Player Action)) e)
    => Eff e (Seq (Action, InGamePlayer))
actionsPerPlayer = do
    actions :: Map Player Action <- ask
    InGamePlayers players        <- gets inGamePlayers
    return $ map
        (\p -> (fromMaybe noAction $ lookup (playerDescription p) actions, p))
        players

-- UTILITY FUNCTIONS

asks :: Member (Reader s) r => (s -> a) -> Eff r a
asks f = map f ask
{-# INLINE asks #-}

gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}
-- TODO: implement with lenses??
