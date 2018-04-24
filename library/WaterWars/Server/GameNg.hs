{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WaterWars.Server.GameNg (runGameTick, gameTick) where

import ClassyPrelude hiding (Reader, ask) -- hide MTL functions reexported by prelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import WaterWars.Core.Physics
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Control.Eff
import WaterWars.Core.PhysicsConstants

runGameTick :: GameState -> Map Player Action -> GameState
runGameTick gameState gameAction =
    run . flip execState gameState . flip runReader gameAction $ gameTick

gameTick
    :: (Member (State GameState) e, Member (Reader (Map Player Action)) e)
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

movePlayers :: (Member (State GameState) e) => Eff e ()
movePlayers = do
    InGamePlayers players <- gets inGamePlayers
    let newPlayers = map movePlayer players
    modify $ \s -> s { inGamePlayers = InGamePlayers newPlayers }

movePlayer :: InGamePlayer -> InGamePlayer
movePlayer (player@InGamePlayer {..}) =
    player { playerLocation = moveLocation playerVelocity playerLocation }

-- | Applies the actions given for each player to the player-obects
applyActionsToPlayers
    :: (Member (State GameState) e, Member (Reader (Map Player Action)) e)
    => Eff e ()
applyActionsToPlayers = do
    perPlayer <- actionsPerPlayer
    let modifiedPlayers = map modifyPlayerByAction perPlayer
    modify $ \s -> s { inGamePlayers = InGamePlayers modifiedPlayers }

gravityToPlayer :: InGamePlayer -> InGamePlayer
gravityToPlayer p@InGamePlayer {..} = p
    { playerVelocity = VelocityVector (vx - gravityForce) vy
    }
    where VelocityVector vx vy = playerVelocity

-- | Function that includes the actions into a player-state
-- TODO improve action type & implementation of this function
modifyPlayerByAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByAction action =
    modifyPlayerByRunAction action . modifyPlayerByJumpAction action

modifyPlayerByJumpAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByJumpAction action player = fromMaybe player $ do -- maybe monad
    JumpAction jumpDirection <- jumpAction action
    return player { playerVelocity = jumpVector ++ playerVelocity player }

modifyPlayerByRunAction :: Action -> InGamePlayer -> InGamePlayer
modifyPlayerByRunAction action player = fromMaybe player $ do -- maybe monad
    RunAction runDirection <- runAction action
    return player
        { playerVelocity = runVelocityVector runDirection
            ++ playerVelocity player
        }

-- get action / player tuples
actionsPerPlayer
    :: (Member (State GameState) e, Member (Reader (Map Player Action)) e)
    => Eff e (Seq (InGamePlayer, Action))
actionsPerPlayer = do
    actions :: Map Player Action <- ask
    InGamePlayers players        <- gets inGamePlayers
    return $ map
        (\p -> (p, fromMaybe noAction $ lookup (playerDescription p) actions))
        players

-- UTILITY FUNCTIONS

gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}
-- TODO: implement with lenses??
