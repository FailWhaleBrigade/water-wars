{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

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
import           WaterWars.Core.Game.Constants
import           WaterWars.Core.Physics
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.Physics.Utils
import           Effectful.State.Dynamic as State
import           Effectful.Reader.Static as Reader
import           Effectful.Writer.Dynamic
import           Effectful
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
        . runPureEff
        . runStateLocal gameState
        . execWriterLocal @[GameEvent]
        . runReader gameMap
        . runReader gameAction
        . runReader gameRunning
        $ gameTick

gameTick
    :: ( State GameState :> e
       , Reader (Map Player Action) :> e
       , Reader GameMap :> e
       , Reader Bool :> e
       , Writer [GameEvent] :> e
       ) -- TODO: better type for that
    => Eff e ()
gameTick = do
    mapMOverPlayers modifyPlayerByAction
    mapMOverPlayers modifyPlayerByEnvironment
    mapMOverProjectiles modifyProjectileByEnvironment
    filterMOverProjectiles boundProjectile
    mapMOverProjectiles (return . moveProjectile)
    checkProjectilePlayerCollision
    checkPlayerOutOfMap
    mapMOverPlayers movePlayer
    modify incrementGameTick

-- | Function that includes the actions into a player-state
modifyPlayerByAction
    :: ( State GameState :> e
       , Reader (Map Player Action) :> e
       , Reader GameMap :> e
       , Writer [GameEvent] :> e
       )
    => InGamePlayer
    -> Eff e InGamePlayer
modifyPlayerByAction player = execStateLocal player $ do
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

boundProjectile :: Reader GameMap :> e => Projectile -> Eff e Bool
boundProjectile Projectile {..} = do
    terrain <- Reader.asks gameTerrain
    let block            = getApproximateBlock projectileLocation
    let mapBounds        = bounds . terrainBlocks $ terrain
    let inBounds         = inRange mapBounds block
    let entersSolidBlock = isSolidAt terrain block

    return $ not entersSolidBlock && inBounds

-- apply any shoot action, if possible
doShootAction
    :: ( State GameState :> e
       , State InGamePlayer :> e
       , Writer [GameEvent] :> e
       )
    => Action
    -> Eff e ()
doShootAction Action { shootAction } = do
    p@InGamePlayer {..} <- get
    when (playerShootCooldown == 0) $ whenJust shootAction $ \angle -> do
        let newProjectile = newProjectileFromAngle p angle
        tell [ShotProjectile newProjectile]
        addProjectile newProjectile
        modify setPlayerCooldown

-- do gravity, bounding, ...
modifyPlayerByEnvironment
    :: Reader GameMap :> r => InGamePlayer -> Eff r InGamePlayer
modifyPlayerByEnvironment p = do
    isOnGround <- isPlayerOnGround p
    return
        . modifyPlayerVelocity (boundVelocityVector maxVelocity)
        . verticalDragPlayer isOnGround
        . gravityPlayer isOnGround
        $ p

modifyProjectileByEnvironment :: Projectile -> Eff r Projectile
modifyProjectileByEnvironment = return -- . modifyProjectileVelocity (boundVelocityVector maxVelocity)

checkPlayerOutOfMap
    :: (State GameState :> r, Reader GameMap :> r) => Eff r ()
checkPlayerOutOfMap = do
    players :: [InGamePlayer] <- State.gets
        (toList . getInGamePlayers . inGamePlayers)
    currentTick <- State.gets gameTicks

    (BlockLocation (minX, minY), BlockLocation (maxX, maxY)) <- Reader.asks
        (bounds . terrainBlocks . gameTerrain)

    let outOfBoundsPlayers =
            [ p
            | p@InGamePlayer { playerLocation = Location (x, y) } <- players
            , let outXMin = x < fromIntegral minX - outOfBoundsTolerance
            , let outXMax = fromIntegral maxX + outOfBoundsTolerance < x
            , let outYMin = y < fromIntegral minY - outOfBoundsTolerance
            , let outYMax = fromIntegral maxY + outOfBoundsTolerance < y
            , outXMin || outXMax || outYMin || outYMax
            ]

    modify
        ( removePlayers
        $ setFromList
        . map playerDescription
        $ outOfBoundsPlayers
        )

    let deadPlayers = map (newDeadPlayer currentTick) outOfBoundsPlayers
    addDeadPlayers deadPlayers


-- TODO: test contained code
checkProjectilePlayerCollision
    :: (State GameState :> r, Reader Bool :> r) => Eff r ()
checkProjectilePlayerCollision = do
    isGameRunning <- ask
    when isGameRunning $ do
        players :: [InGamePlayer] <- State.gets
            (toList . getInGamePlayers . inGamePlayers)
        projectiles :: [Projectile] <- State.gets
            (toList . getProjectiles . gameProjectiles)
        currentTick <- State.gets gameTicks

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
