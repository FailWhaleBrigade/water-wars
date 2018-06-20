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

import           WaterWars.Core.Game
import           WaterWars.Core.Physics
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.Physics.Utils
import           Control.Eff.State.Strict
import           Control.Eff.Reader.Strict
import           Control.Eff
import           Data.Array.IArray
import           Control.Monad.Extra                      ( whenJust )

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
    mapMOverPlayers modifyPlayerByEnvironment
    mapMOverProjectiles modifyProjectileByEnvironment
    filterMOverProjectiles boundProjectile
    mapMOverProjectiles (return . moveProjectile)
    checkProjectilePlayerCollision
    mapMOverPlayers movePlayer
    modify incrementGameTick
    return ()

-- | Function that includes the actions into a player-state
modifyPlayerByAction
    :: ( Member (State GameState) e
       , Member (Reader (Map Player Action)) e
       , Member (Reader GameMap) e
       )
    => InGamePlayer
    -> Eff e InGamePlayer
modifyPlayerByAction player = execState player $ do
    actionMap :: Map Player Action <- ask
    isOnGround                     <- isPlayerOnGround player
    let action =
            fromMaybe noAction $ lookup (playerDescription player) actionMap
    doShootAction action
    modify
        ( modifyPlayerShootCooldown
        . modifyPlayerByRunAction isOnGround action -- TODO: use local reader here?
        . modifyPlayerByJumpAction isOnGround action
        )

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
            player { playerLastRunDirection = runDirection }

-- decrease cooldown
modifyPlayerShootCooldown :: InGamePlayer -> InGamePlayer
modifyPlayerShootCooldown player@InGamePlayer {..} =
    if playerShootCooldown == 0
        then player
        else player { playerShootCooldown = playerShootCooldown - 1 }

boundProjectile :: Member (Reader GameMap) e => Projectile -> Eff e Bool
boundProjectile Projectile {..} = do
    terrain <- asks gameTerrain
    let block            = getBlock projectileLocation
    let mapBounds        = bounds . terrainBlocks $ terrain
    let inBounds         = inRange mapBounds block
    let entersSolidBlock = isSolidAt terrain block

    return $ not entersSolidBlock && inBounds

-- apply any shoot action, if possible
doShootAction
    :: (Member (State GameState) e, Member (State InGamePlayer) e)
    => Action
    -> Eff e ()
doShootAction Action { shootAction } = do
    p@InGamePlayer {..} <- get
    when (playerShootCooldown == 0) $ whenJust shootAction $ \angle -> do
        addProjectile $ newProjectileFromAngle (playerHeadLocation p) angle
        modify setPlayerCooldown

-- do gravity, bounding, ...
modifyPlayerByEnvironment
    :: Member (Reader GameMap) r => InGamePlayer -> Eff r InGamePlayer
modifyPlayerByEnvironment p = do
    isOnGround <- isPlayerOnGround p
    return
        . modifyPlayerVelocity (boundVelocityVector maxVelocity)
        . verticalDragPlayer isOnGround
        . gravityPlayer isOnGround
        $ p

modifyProjectileByEnvironment :: Projectile -> Eff r Projectile
modifyProjectileByEnvironment =
    return
        . modifyProjectileVelocity (boundVelocityVector maxVelocity)
        -- . gravityProjectile

checkProjectilePlayerCollision :: (Member (State GameState) r) => Eff r ()
checkProjectilePlayerCollision = do
    players <- gets inGamePlayers
    projectiles <- gets gameProjectiles

    return () -- TODO: implement collision check, remove players and projectiles
