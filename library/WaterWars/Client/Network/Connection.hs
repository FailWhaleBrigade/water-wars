{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Client.Network.Connection
    ( module WaterWars.Client.Network.State
    , module WaterWars.Client.Network.Connection
    )
where

import           ClassyPrelude

import qualified Network.WebSockets            as WS

import           Control.Monad.Logger

import           Control.Concurrent

import           WaterWars.Client.Render.State
import           WaterWars.Client.Network.State           ( NetworkConfig(..)
                                                          , NetworkInfo(..)
                                                          , Connection
                                                          , newConnection
                                                          )

import           WaterWars.Network.Protocol    as Protocol
import           WaterWars.Network.Connection

import           WaterWars.Core.Game           as CoreState

connectionThread
    :: MonadIO m => Maybe NetworkInfo -> NetworkConfig -> WorldSTM -> m ()
connectionThread _ NetworkConfig {..} world = forever $ do
    ret :: Either SomeException () <- liftIO $ try $ WS.runClient
        hostName
        portId
        ""
        (\conn -> do
            say "Connection has been opened"
            let connection = newConnection conn
            -- TODO: this setup code should be refactored soon-ish
            send connection (LoginMessage (Login Nothing))
            _ <- async $ receiveUpdates world connection
            sendUpdates world connection
        )

    --case ret of
    --    Left (WS.CloseRequest _ _) ->
    --        say "Server requested to close the conenction"
    --    Left WS.ConnectionClosed     -> say "Connection has been closed"
    --    Left (WS.ParseException   _) -> say "Client sent impressive garbage"
    --    Left (WS.UnicodeException _) -> say "Weird unicode error"
    --    Right () ->
    --        say
    --            "Altoough weird, the connection was a success, whatever that means"
    say "Connection failed, retry in some time"
    liftIO $ threadDelay (1000000 * 5)



receiveUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
receiveUpdates (WorldSTM tvar) conn =
    runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ forever
        $ do
              $logDebug "Wait for Game Update"
              serverMsg <- receive conn
              case serverMsg of
                  Left msg_ ->
                      $logWarn $ "Could not read message: " ++ tshow msg_

                  Right msg -> atomically $ do
                      world <- readTVar tvar
                      let world' = updateWorld msg world
                      writeTVar tvar world'

              return ()

sendUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
sendUpdates (WorldSTM tvar) conn =
    runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ forever
        $ do
              $logDebug "Send an update to the Server"
              (action, world) <- atomically $ do
                  action <- extractGameAction tvar
                  world  <- readTVar tvar
                  -- If the player wants to ready up, the message will be sent
                  -- outside of the STM block.
                  -- In order to avoid multiple sending of the message,
                  -- we set the readyUp key press to false.
                  when
                      (readyUp $ worldInfo world)
                      (writeTVar
                          tvar
                          (world
                              { worldInfo = (worldInfo world) { readyUp = False
                                                              }
                              }
                          )
                      )
                  return (action, world)
              $logDebug $ "Message: " ++ tshow action
              send conn (PlayerActionMessage action)
              when (readyUp $ worldInfo world)
                   (send conn (ClientReadyMessage ClientReady))
              liftIO $ threadDelay (1000000 `div` 80)
              return ()


updateWorld :: Protocol.ServerMessage -> World -> World
updateWorld serverMsg world@World {..} = case serverMsg of
    GameMapMessage gameMap ->
        setTerrain (blockMap $ resources renderInfo) (gameTerrain gameMap) world

    GameStateMessage gameState@GameState {..} gameEvents ->
        let WorldInfo {..} = worldInfo
            {--
            --inGamePlayers_ :: Seq InGamePlayer
            --inGamePlayers_ = getInGamePlayers inGamePlayers

            --deadPlayers_ :: Seq DeadPlayer
            --deadPlayers_ = getDeadPlayers gameDeadPlayers
            --}
            newProjectiles :: Seq Projectile
            newProjectiles = getProjectiles gameProjectiles

            worldInfo_     = WorldInfo {projectiles = newProjectiles, ..}
        in  World
                { worldInfo      = worldInfo_
                , lastGameUpdate = ServerUpdate gameState
                , ..
                }

    GameSetupResponseMessage _ -> world

    LoginResponseMessage loginResponse ->
        let WorldInfo {..} = worldInfo
            newPlayer = Just (playerDescription $ successPlayer loginResponse)
            worldInfo_ = WorldInfo {localPlayer = newPlayer, ..}
        in  World {worldInfo = worldInfo_, ..}

    GameWillStartMessage (GameStart n) ->
        world { worldInfo = worldInfo { countdown = Just n } }

    GameStartMessage -> world { worldInfo = worldInfo { countdown = Nothing } }


extractGameAction :: TVar World -> STM Protocol.PlayerAction
extractGameAction worldTvar = do
    world <- readTVar worldTvar
    let WorldInfo {..} = worldInfo world
    let GameState {..} = gameStateUpdate $ lastGameUpdate world
    let runCmd | walkLeft  = Just (RunAction RunLeft)
               | walkRight = Just (RunAction RunRight)
               | otherwise = Nothing
    let jmpCmd = if jump then Just JumpAction else Nothing
    let shootCmd = do -- maybe monad
            shootTarget  <- shoot
            player       <- localPlayer
            inGamePlayer <- find ((== player) . playerDescription)
                                 (getInGamePlayers inGamePlayers)
            let shootLocation = playerHeadLocation inGamePlayer
            return $ calculateAngle shootLocation shootTarget
    when (isJust shoot) $ writeTVar worldTvar $ world
        { worldInfo = WorldInfo {shoot = Nothing, ..}
        }

    let playerAction = Action
            { runAction   = runCmd
            , jumpAction  = jmpCmd
            , shootAction = shootCmd
            }
    return PlayerAction {getAction = playerAction}


calculateAngle :: Location -> Location -> Angle
calculateAngle (Location (x1, y1)) (Location (x2, y2)) =
    Angle (atan2 (y2 - y1) (x2 - x1))
