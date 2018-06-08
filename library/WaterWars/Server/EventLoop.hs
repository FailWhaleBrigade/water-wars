module WaterWars.Server.EventLoop where

import           ClassyPrelude

import           WaterWars.Network.Protocol
import           WaterWars.Core.Game
import           WaterWars.Server.ConnectionMgnt

eventLoop
    :: MonadIO m
    => TChan EventMessage
    -> TVar GameLoopState
    -> TVar PlayerActions
    -> TVar (Map Text ClientConnection)
    -> TVar (Map Text InGamePlayer)
    -> m ()
eventLoop broadcastChan gameLoopTvar playerActionTvar sessionMapTvar playerMapTVar
    = forever $ do
        message <- atomically $ readTChan broadcastChan

        case message of
            -- handle messages sent by a client connection
            EventClientMessage sessionId clientMsg -> handleClientMessages
                sessionId
                clientMsg
                gameLoopTvar
                playerActionTvar
                sessionMapTvar
                playerMapTVar

            -- handle messages sent from the gameloop
            EventGameLoopMessage gameStateUpdate ->
                handleGameLoopMessages gameStateUpdate sessionMapTvar

        -- TODO: handle messages sent from websocket app 

handleGameLoopMessages
    :: MonadIO m => GameState -> TVar (Map Text ClientConnection) -> m ()
handleGameLoopMessages gameStateUpdate sessionMapTvar = do
    sessionMap <- readTVarIO sessionMapTvar
    forM_ sessionMap $ \conn -> atomically
        $ writeTChan (readChannel conn) (GameStateMessage gameStateUpdate)

handleClientMessages
    :: MonadIO m
    => Text
    -> ClientMessage
    -> TVar GameLoopState
    -> TVar PlayerActions
    -> TVar (Map Text ClientConnection)
    -> TVar (Map Text InGamePlayer)
    -> m ()
handleClientMessages sessionId clientMsg gameLoopTvar playerActionTvar sessionMapTvar playerMapTVar
    = case clientMsg of
        LoginMessage _ -> do
            -- TODO: Handle reconnects
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
            return ()

        LogoutMessage Logout -> do
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
            return ()

        GameSetupMessage _ ->
            -- TODO: we dont handle game setup requests yet
            return ()

        PlayerActionMessage playerAction -> do
            playerMay <- lookup sessionId <$> readTVarIO playerMapTVar
            case playerMay of
                Nothing                -> return () -- TODO: add logging facilities
                Just InGamePlayer {..} -> atomically $ do
                    PlayerActions {..} <- readTVar playerActionTvar
                    let getPlayerActions' = insertWith
                            (++)
                            playerDescription
                            (getAction playerAction)
                            getPlayerActions
                    writeTVar playerActionTvar (PlayerActions getPlayerActions')
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
    , playerWidth            = 0.95
    , playerHeight           = 1.52
    }
