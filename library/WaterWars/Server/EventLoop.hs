module WaterWars.Server.EventLoop where

import ClassyPrelude

import System.Log.Logger

import WaterWars.Network.Protocol
import WaterWars.Network.Connection

import WaterWars.Core.GameState
import WaterWars.Server.ConnectionMgnt

eventLoop
    :: MonadIO m
    => TChan (ClientMessage, Text)
    -> TVar ServerState
    -> TVar PlayerActions
    -> Map Text InGamePlayer
    -> m ()
eventLoop broadcastChan tvar playerActionTvar sessionMap = do
    (clientMsg, sessionId) <- atomically $ readTChan broadcastChan
    liftIO $ debugM "Server.Connection"
                    ("Successfully read a message from: " ++ show sessionId)
    sessionMap' <- case clientMsg of
        LoginMessage _ -> do
            -- TODO: Handle reconnects
            let player = newInGamePlayer (Player sessionId) (Location (0, 0))
            (connectionMay, gameMap_) <- atomically $ do
                serverState <- readTVar tvar
                let serverState' = modifyGameState addPlayer serverState player
                writeTVar tvar serverState'
                return
                    ( getConnectionBySessionId (connections serverState)
                                               sessionId
                    , gameMap serverState
                    )
            case connectionMay of
                Nothing         -> return ()
                Just connection -> do
                    -- TODO: maybe batch these message if possible
                    sendTo
                        connection
                        (LoginResponseMessage (LoginResponse sessionId player))
                    sendTo connection (GameMapMessage gameMap_)
            return $ insertMap sessionId player sessionMap

        LogoutMessage _ -> do
            -- TODO: why do we have a session id? 
            -- TODO: remove connections
            let playerMay = lookup sessionId sessionMap
            case playerMay of
                Nothing     -> return ()
                Just player -> atomically $ do
                    serverState <- readTVar tvar
                    let serverState' =
                            modifyGameState removePlayer serverState player
                    writeTVar tvar serverState'
            return $ deleteMap sessionId sessionMap

        GameSetupMessage gameSetup ->
            -- TODO: we dont handle game setup requests yet
            return sessionMap

        PlayerActionMessage playerAction -> do
            let playerMay = lookup sessionId sessionMap
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
            return sessionMap

    eventLoop broadcastChan tvar playerActionTvar sessionMap'


-- utility functions for creation
newInGamePlayer :: Player -> Location -> InGamePlayer
newInGamePlayer player location = InGamePlayer
    { playerDescription   = player
    , playerLocation      = location
    , playerHealth        = 10
    , playerMaxHealth     = 10
    , playerViewDirection = Angle 0.0
    , playerVelocity      = VelocityVector 0 0
    }

-- utility functions to retrieve connections
getConnectionBySessionId :: Connections -> Text -> Maybe ClientConnection
getConnectionBySessionId conns sessionId = lookup sessionId (players conns)

