{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Client.Network.Connection
    ( module WaterWars.Client.Network.State
    , module WaterWars.Client.Network.Connection
    )
where

import           ClassyPrelude

import qualified Network.WebSockets            as WS
import           Sound.ProteaAudio
import           Control.Monad.Logger

import           Control.Concurrent

import           WaterWars.Client.Render.State
import           WaterWars.Client.Network.State ( NetworkConfig(..)
                                                , NetworkInfo(..)
                                                , Connection
                                                , newConnection
                                                , send
                                                , receive
                                                )

import           WaterWars.Network.Protocol    as Protocol

import           WaterWars.Core.Game           as CoreState

connectionThread
    :: MonadIO m => Maybe NetworkInfo -> NetworkConfig -> WorldSTM -> m ()
connectionThread _ NetworkConfig {..} world = forever $ do
    _ :: Either SomeException () <-
        liftIO
        $ try
        $ WS.runClient
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

                  Right msg -> do
                      (world', events) <- atomically $ do
                          world <- readTVar tvar
                          let (world', maybeEvents) = updateWorld msg world
                          writeTVar tvar world'
                          return (world', maybeEvents)
                      when (isJust events) $ liftIO $ soundPlay
                          (shootSound . resources $ renderInfo world')
                          1
                          1
                          0
                          1

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


updateWorld :: Protocol.ServerMessage -> World -> (World, Maybe GameEvents)
updateWorld serverMsg world@World {..} = case serverMsg of
    GameMapMessage gameMap ->
        ( setTerrain (terrainDecoration gameMap) (gameTerrain gameMap) world
        , Nothing
        )
    GameStateMessage gameState@GameState {..} gameEvents ->
        let
            WorldInfo {..} = worldInfo

            newProjectiles :: Seq Projectile
            newProjectiles = getProjectiles gameProjectiles

            worldInfo_     = WorldInfo {projectiles = newProjectiles, ..}
            maybeEvents    = if null $ getGameEvents gameEvents
                then Nothing
                else Just gameEvents
        in
            ( World
                { worldInfo      = worldInfo_
                , lastGameUpdate = ServerUpdate gameState
                , ..
                }
            , maybeEvents
            )

    GameSetupResponseMessage _ -> (world, Nothing)

    LoginResponseMessage loginResponse ->
        let WorldInfo {..} = worldInfo
            newPlayer = Just (playerDescription $ successPlayer loginResponse)
            worldInfo_ = WorldInfo {localPlayer = newPlayer, ..}
        in  (World {worldInfo = worldInfo_, ..}, Nothing)

    GameWillStartMessage (GameStart n) ->
        (world { worldInfo = worldInfo { countdown = Just n } }, Nothing)

    GameStartMessage ->
        ( world
            { worldInfo = worldInfo { countdown = Nothing, gameRunning = True }
            }
        , Nothing
        )
    ResetGameMessage ->
        -- TODO: this is kind of hacky, we just forget the last game update to avoid the race condition
        -- between deleting all animation and the next gloss update which generates new animation as needed
        ( world
            { renderInfo     = renderInfo { playerAnimations = mapFromList [] }
            , worldInfo      = worldInfo { winnerPlayer = Nothing }
            , lastGameUpdate = ServerUpdate
                                   { gameStateUpdate = GameState
                                       { inGamePlayers   = InGamePlayers empty
                                       , gameDeadPlayers = DeadPlayers empty
                                       , gameProjectiles = Projectiles empty
                                       , gameTicks       = 0
                                       }
                                   }
            }
        , Nothing
        )
    StopGame ->
        ( world
            { worldInfo = worldInfo { countdown    = Nothing
                                    , gameRunning  = False
                                    , winnerPlayer = Nothing
                                    }
            }
        , Nothing
        )
    StopGameWithWinner winner ->
        ( world
            { worldInfo = worldInfo { countdown    = Nothing
                                    , gameRunning  = False
                                    , winnerPlayer = Just winner
                                    }
            }
        , Nothing
        )


extractGameAction :: TVar World -> STM Protocol.PlayerAction
extractGameAction worldTvar = do
    world <- readTVar worldTvar
    let WorldInfo {..} = worldInfo world
    let GameState {..} = gameStateUpdate $ lastGameUpdate world
    let runCmd | walkLeft  = Just (RunAction RunLeft)
               | walkRight = Just (RunAction RunRight)
               | otherwise = Nothing
    let jmpCmd = if jump then Just JumpAction else Nothing
    let shootCmd = do -- Maybe Shoot
            shootTarget  <- shoot
            player       <- localPlayer
            inGamePlayer <- find ((== player) . playerDescription)
                                 (getInGamePlayers inGamePlayers)
            let shootLocation = playerHeadLocation inGamePlayer
            return $ calculateAngle shootLocation shootTarget
    when (isJust shoot) $ writeTVar worldTvar $ world
        { worldInfo = WorldInfo {shoot = Nothing, lastShot = shoot, ..}
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
