{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WaterWars.Core.GameNg
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
import           Control.Eff.Writer.Strict
import           Control.Eff
import           Data.Array.IArray
import           Control.Monad.Extra                      ( whenJust )

runGameTick
    :: Bool
    -> GameMap
    -> GameState
    -> Map Player Action
    -> (GameEvents, GameState)
runGameTick gameRunning gameMap gameState gameAction =
    first (GameEvents . fromList)
        . run
        . runState gameState
        . execListWriter
        . runReader gameMap
        . runReader gameAction
        . runReader gameRunning
        $ gameTick

gameTick
    :: ( Member (State GameState) e
       , Member (Reader (Map Player Action)) e
       , Member (Reader GameMap) e
       , Member (Reader Bool) e
       , Member (Writer GameEvent) e
       ) -- TODO: better type for that
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

-- | Function that includes the actions into a player-state
modifyPlayerByAction
    :: ( Member (State GameState) e
       , Member (Reader (Map Player Action)) e
       , Member (Reader GameMap) e
       , Member (Writer GameEvent) e
       )
    => InGamePlayer
    -> Eff e InGamePlayer
modifyPlayerByAction player = execState player $ do
    actionMap :: Map Player Action <- ask
    isOnGround                     <- isPlayerOnGround player
    let action =
            fromMaybe noAction $ lookup (playerDescription player) actionMap
    doShootAction action

    runsAgainsWall <- doesPlayerRunAgainstWall (runAction action) player
    modify
        ( modifyPlayerShootCooldown
        . modifyPlayerByRunAction isOnGround runsAgainsWall action -- TODO: use local reader here?
        . modifyPlayerByJumpAction isOnGround action
        )

modifyPlayerByJumpAction :: Bool -> Action -> InGamePlayer -> InGamePlayer
modifyPlayerByJumpAction onGround action player@InGamePlayer {..} =
    fromMaybe player $ do -- maybe monad
        unless onGround Nothing
        JumpAction <- jumpAction action
        return $ setPlayerVelocity (jumpVector playerVelocity) player

modifyPlayerByRunAction
    :: Bool -> Bool -> Action -> InGamePlayer -> InGamePlayer
modifyPlayerByRunAction onGround runsAgainstWall action player@InGamePlayer {..}
    = fromMaybe player $ do -- maybe monad
        guard (not runsAgainstWall)
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
    let block            = getApproximateBlock projectileLocation
    let mapBounds        = bounds . terrainBlocks $ terrain
    let inBounds         = inRange mapBounds block
    let entersSolidBlock = isSolidAt terrain block

    return $ not entersSolidBlock && inBounds

-- apply any shoot action, if possible
doShootAction
    :: ( Member (State GameState) e
       , Member (State InGamePlayer) e
       , Member (Writer GameEvent) e
       )
    => Action
    -> Eff e ()
doShootAction Action { shootAction } = do
    p@InGamePlayer {..} <- get
    when (playerShootCooldown == 0) $ whenJust shootAction $ \angle -> do
        let newProjectile = newProjectileFromAngle p angle
        tell $ ShotProjectile newProjectile
        addProjectile newProjectile
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
modifyProjectileByEnvironment = return -- . modifyProjectileVelocity (boundVelocityVector maxVelocity)

-- TODO: test contained code
checkProjectilePlayerCollision
    :: (Member (State GameState) r, Member (Reader Bool) r) => Eff r ()
checkProjectilePlayerCollision = do
    isGameRunning <- ask
    when isGameRunning $ do
        players :: [InGamePlayer] <- gets
            (toList . getInGamePlayers . inGamePlayers)
        projectiles :: [Projectile] <- gets
            (toList . getProjectiles . gameProjectiles)
        currentTick <- gets gameTicks

        let (hitPlayers, hitProjectiles) = unzip
                [ (player, projectile)
                | player     <- players
                , projectile <- projectiles
                , projectilePlayer projectile /= playerDescription player
                , getsHit player projectile
                ]
        modify
            (removePlayers $ setFromList . map playerDescription $ hitPlayers)
        removeProjectiles $ setFromList hitProjectiles

        let deadPlayers = map (newDeadPlayer currentTick) hitPlayers
        addDeadPlayers deadPlayers
