{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Server.EventLoop where

import ClassyPrelude

import Control.Concurrent (threadDelay)
import Control.Monad.Logger

import WaterWars.Network.Protocol
import WaterWars.Core.Game
import WaterWars.Server.ConnectionMgnt

eventLoop
    :: (MonadIO m, MonadLogger m)
    => TChan EventMessage
    -> TVar GameLoopState
    -> TVar PlayerActions
    -> TVar (Map Text ClientConnection)
    -> TVar (Map Text InGamePlayer)
    -> TVar (Set Text)
    -> m ()
eventLoop broadcastChan gameLoopTvar playerActionTvar sessionMapTvar playerMapTVar readyPlayers
    = forever $ do
        message <- atomically $ readTChan broadcastChan
        $logDebug $ "Read a new event loop message: " ++ tshow message
        case message of
            -- handle messages sent by a client connection
            EventClientMessage sessionId clientMsg -> handleClientMessages
                sessionId
                clientMsg
                gameLoopTvar
                playerActionTvar
                sessionMapTvar
                playerMapTVar
                readyPlayers

            -- handle messages sent from the gameloop
            EventGameLoopMessage gameStateUpdate ->
                handleGameLoopMessages gameStateUpdate sessionMapTvar

        -- TODO: handle messages sent from websocket app

handleGameLoopMessages
    :: (MonadIO m, MonadLogger m)
    => GameState
    -> TVar (Map Text ClientConnection)
    -> m ()
handleGameLoopMessages gameStateUpdate sessionMapTvar = do
    sessionMap <- readTVarIO sessionMapTvar
    broadcastMessage (GameStateMessage gameStateUpdate) sessionMap

handleClientMessages
    :: (MonadIO m, MonadLogger m)
    => Text
    -> ClientMessage
    -> TVar GameLoopState
    -> TVar PlayerActions
    -> TVar (Map Text ClientConnection)
    -> TVar (Map Text InGamePlayer)
    -> TVar (Set Text)
    -> m ()
handleClientMessages sessionId clientMsg gameLoopTvar playerActionTvar sessionMapTvar playerMapTVar readyPlayers
    = case clientMsg of
        LoginMessage _ -> do
            -- TODO: Handle reconnects
            $logInfo ("Login message from \"" ++ sessionId ++ "\"")
            -- Create new Player
            let player = newInGamePlayer (Player sessionId) (Location (0, 0))
            -- Tell the game loop engine about the newly connected player
            (connectionMay, gameMap_) <- atomically $ do
                serverState <- readTVar gameLoopTvar
                let serverState' = modifyGameState addPlayer serverState player
                writeTVar gameLoopTvar serverState'
                sessionMap <- readTVar sessionMapTvar
                modifyTVar' playerMapTVar (insertMap sessionId player)
                return (lookup sessionId sessionMap, gameMap serverState)
            -- If a connection is found then send required information
            case connectionMay of
                Nothing         -> return ()
                Just connection -> atomically $ do
                    writeTChan
                        (readChannel connection)
                        (LoginResponseMessage (LoginResponse sessionId player))
                    writeTChan (readChannel connection)
                               (GameMapMessage gameMap_)
                    writeTChan (readChannel connection)
                               (GameMapMessage gameMap_)
            return ()

        LogoutMessage Logout -> do
            $logInfo ("Logout message from \"" ++ sessionId ++ "\"")
            playerMay <- lookup sessionId <$> readTVarIO playerMapTVar
            case playerMay of
                Nothing     -> return ()
                Just player -> atomically $ do
                    serverState <- readTVar gameLoopTvar
                    let serverState' =
                            modifyGameState removePlayer serverState player
                    writeTVar   gameLoopTvar   serverState'
                    modifyTVar' sessionMapTvar (deleteMap sessionId)
                    modifyTVar' playerMapTVar  (deleteMap sessionId)
                    modifyTVar' readyPlayers   (deleteSet sessionId)
            return ()

        GameSetupMessage _ ->
            -- TODO: we dont handle game setup requests yet
            return ()

        PlayerActionMessage playerAction -> do
            playerMay <- lookup sessionId <$> readTVarIO playerMapTVar
            case playerMay of
                Nothing -> do
                    $logWarn
                        "Received a message that did not belong to any player"
                    return ()
                Just InGamePlayer {..} -> atomically $ do
                    PlayerActions {..} <- readTVar playerActionTvar
                    let getPlayerActions' = insertWith
                            (++)
                            playerDescription
                            (getAction playerAction)
                            getPlayerActions
                    writeTVar playerActionTvar (PlayerActions getPlayerActions')
            return ()

        ClientReadyMessage ClientReady -> do
            $logInfo ("Player \"" ++ sessionId ++ "\" is ready")
            allPlayersReady <- atomically $ do
                readySet  <- readTVar readyPlayers
                playerMap <- readTVar playerMapTVar
                if member sessionId readySet
                    then return False
                    else do
                        let readySet' = insertSet sessionId readySet
                        writeTVar readyPlayers readySet'
                        if keysSet playerMap == readySet'
                            then do
                                modifyTVar' gameLoopTvar startGame
                                return True
                            else return False

            when allPlayersReady $ do
                $logInfo
                    (  "Everyone is ready. \""
                    ++ sessionId
                    ++ "\" was the last one."
                    )
                sessionMap <- readTVarIO sessionMapTvar
                broadcastMessage (GameStartMessage (GameStart 5)) sessionMap
                -- Start the game in 5 seconds
                _ <- liftIO $ async $ do
                    threadDelay (1000000 * 5)
                    atomically (modifyTVar' gameLoopTvar startGame)
                    return ()
                return ()

            return ()

-- utility functions for creation
newInGamePlayer :: Player -> Location -> InGamePlayer
newInGamePlayer player location = InGamePlayer
    { playerDescription      = player
    , playerLocation         = location
    , playerHealth           = 10
    , playerMaxHealth        = 10
    , playerLastRunDirection = RunLeft
    , playerVelocity         = VelocityVector 0 0
    , playerShootCooldown    = 0
    , playerWidth            = newPlayerWidth
    , playerHeight           = newPlayerHeight
    }
  where
    newPlayerWidth  = 2
    newPlayerHeight = 1.6 * newPlayerWidth

broadcastMessage
    :: MonadIO m => ServerMessage -> Map Text ClientConnection -> m ()
broadcastMessage serverMessage sessionMap = forM_ sessionMap
    $ \conn -> atomically $ writeTChan (readChannel conn) serverMessage
