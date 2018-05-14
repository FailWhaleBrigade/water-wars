{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WaterWars.Server.GameNg
    ( runGameTick
    , gameTick
    )
where

import           ClassyPrelude                     hiding ( Reader
                                                          , ask
                                                          , asks
                                                          )
import           WaterWars.Core.GameState
import           WaterWars.Core.GameMap
import           WaterWars.Core.GameUtils
import           WaterWars.Core.GameAction
import           WaterWars.Core.Physics
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.Physics.Utils
import           Control.Eff.State.Strict
import           Control.Eff.Reader.Strict
import           Control.Eff
-- import           Control.Monad.Extra                      ( whenJust )

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
    mapMOverProjectiles (return . moveProjectile) -- TODO: clean projectile
    mapMOverPlayers modifyPlayerByEnvironment
    mapMOverPlayers movePlayer
    return ()

-- | Function that includes the actions into a player-state
modifyPlayerByAction
    :: ( Member (State GameState) e
       , Member (Reader (Map Player Action)) e
       , Member (Reader GameMap) e
       )
    => InGamePlayer
    -> Eff e InGamePlayer
modifyPlayerByAction player = do
    actionMap :: Map Player Action <- ask
    let action =
            fromMaybe noAction $ lookup (playerDescription player) actionMap
    isOnGround <- isPlayerOnGround player -- TODO: deduplicate
    player'    <- doShootAction action player -- TODO: local state for player?
    return
        . modifyPlayerShootCooldown
        . modifyPlayerByRunAction isOnGround action
        . modifyPlayerByJumpAction isOnGround action
        $ player'

modifyPlayerByJumpAction :: Bool -> Action -> InGamePlayer -> InGamePlayer
modifyPlayerByJumpAction onGround action player@InGamePlayer {..} =
    fromMaybe player $ do -- maybe monad
        unless onGround Nothing
        JumpAction <- jumpAction action
        return $ setPlayerVelocity (jumpVector playerVelocity) player

modifyPlayerByRunAction :: Bool -> Action -> InGamePlayer -> InGamePlayer
modifyPlayerByRunAction onGround action player@InGamePlayer {..} =
    fromMaybe player $ do -- maybe monad
        RunAction runDirection <- runAction action
        return $ setPlayerVelocity
            (  velocityBoundX runSpeed
            $  runVector onGround runDirection
            ++ playerVelocity
            )
            player

-- decrease cooldown
modifyPlayerShootCooldown :: InGamePlayer -> InGamePlayer
modifyPlayerShootCooldown player@InGamePlayer {..} =
    if playerShootCooldown == 0
        then player
        else player { playerShootCooldown = playerShootCooldown - 1 }

-- TODO: local player-state
-- apply any shoot action, if possible
doShootAction
    :: Member (State GameState) e
    => Action
    -> InGamePlayer
    -> Eff e InGamePlayer
doShootAction action player@InGamePlayer {..} =
    case (shootAction action, playerShootCooldown) of
        (Just angle, 0) -> do
            addProjectile $ newProjectileFromAngle playerLocation angle
            return $ player { playerShootCooldown = 20 } -- TODO: game constant
        _ -> return player

-- do gravity, bounding, ...
modifyPlayerByEnvironment
    :: Member (Reader GameMap) r => InGamePlayer -> Eff r InGamePlayer
modifyPlayerByEnvironment p = do
    isOnGround <- isPlayerOnGround p
    return
        . modifyPlayerVelocity (boundVelocityVector maxVelocity)
        . verticalDragPlayer isOnGround
        . gravityPlayer
        $ p
