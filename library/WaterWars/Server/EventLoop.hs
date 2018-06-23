{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Server.EventLoop where

import ClassyPrelude

import Control.Monad.Logger

import WaterWars.Network.Protocol
import WaterWars.Core.Game
import WaterWars.Server.State

eventLoop :: (MonadIO m, MonadLogger m) => SharedState -> m ()
eventLoop sharedState@SharedState {..} = forever $ do
    message <- atomically $ readTChan eventQueue
    $logDebug $ "Read a new event loop message: " ++ tshow message
    case message of
        -- handle messages sent by a client connection
        EventClientMessage sessionId clientMsg ->
            handleClientMessages sharedState sessionId clientMsg

        -- handle messages sent from the gameloop
        EventGameLoopMessage gameStateUpdate ->
            handleGameLoopMessages sharedState gameStateUpdate

        -- TODO: handle messages sent from websocket app

handleGameLoopMessages
    :: (MonadIO m, MonadLogger m) => SharedState -> GameState -> m ()
handleGameLoopMessages SharedState {..} gameStateUpdate = do
    sessionMap <- readTVarIO connectionMapTvar
    gameTick <- gameTicks . gameState <$> readTVarIO gameLoopTvar
    broadcastMessage (GameStateMessage gameStateUpdate) sessionMap
    isStarting <- readTVarIO startGameTvar
    case isStarting of 
        Nothing -> return ()
        Just startingTick -> 
            when (startingTick <= gameTick) $ do
                $logInfo $ "Send the Game start message: " ++ tshow gameTick
                atomically $ do  
                    writeTVar startGameTvar Nothing
                    modifyTVar gameLoopTvar startGame
                
                broadcastMessage GameStartMessage sessionMap

                    
                    
handleClientMessages
    :: (MonadIO m, MonadLogger m)
    => SharedState
    -> Text
    -> ClientMessage
    -> m ()
handleClientMessages SharedState {..} sessionId clientMsg = case clientMsg of
    LoginMessage _ -> do
        -- TODO: Handle reconnects
        $logInfo ("Login message from \"" ++ sessionId ++ "\"")
        -- Create new Player
        let player = newInGamePlayer (Player sessionId) (Location (0, 0))
        -- Tell the game loop engine about the newly connected player
        (connectionMay, gameMap_) <- atomically $ do
            serverState <- readTVar gameLoopTvar
            let serverState' =
                    modifyGameState addInGamePlayer serverState player
            writeTVar gameLoopTvar serverState'
            sessionMap <- readTVar connectionMapTvar
            modifyTVar' playerMapTvar (insertMap sessionId player)
            return (lookup sessionId sessionMap, gameMap serverState)
        -- If a connection is found then send required information
        case connectionMay of
            Nothing         -> return ()
            Just connection -> atomically $ do
                writeTChan
                    (readChannel connection)
                    (LoginResponseMessage (LoginResponse sessionId player))
                writeTChan (readChannel connection) (GameMapMessage gameMap_)
        return ()

    LogoutMessage Logout -> do
        $logInfo ("Logout message from \"" ++ sessionId ++ "\"")
        playerMay <- lookup sessionId <$> readTVarIO playerMapTvar
        case playerMay of
            Nothing     -> return ()
            Just player -> atomically $ do
                serverState <- readTVar gameLoopTvar
                let serverState' = modifyGameState
                        removePlayer
                        serverState
                        (playerDescription player)
                writeTVar   gameLoopTvar      serverState'
                modifyTVar' connectionMapTvar (deleteMap sessionId)
                modifyTVar' playerMapTvar     (deleteMap sessionId)
                modifyTVar' readyPlayersTvar  (deleteSet sessionId)
        return ()

    GameSetupMessage _ ->
        -- TODO: we dont handle game setup requests yet
        return ()

    PlayerActionMessage playerAction -> do
        playerMay <- lookup sessionId <$> readTVarIO playerMapTvar
        case playerMay of
            Nothing -> do
                $logWarn "Received a message that did not belong to any player"
                return ()
            Just InGamePlayer {..} -> atomically $ do
                PlayerActions {..} <- readTVar playerActionTvar
                let getPlayerActions' = insertWith (++)
                                                   playerDescription
                                                   (getAction playerAction)
                                                   getPlayerActions
                writeTVar playerActionTvar (PlayerActions getPlayerActions')
        return ()

    ClientReadyMessage ClientReady -> do
        $logInfo ("Player \"" ++ sessionId ++ "\" is ready")
        allPlayersReady <- atomically $ do
            readySet  <- readTVar readyPlayersTvar
            playerMap <- readTVar playerMapTvar
            -- could be improved with multi way if
            if member sessionId readySet
                then return False
                else do
                    let readySet' = insertSet sessionId readySet
                    writeTVar readyPlayersTvar readySet'
                    if keysSet playerMap == readySet'
                        then do
                            modifyTVar' gameLoopTvar startGame
                            return True
                        else return False

        when allPlayersReady $ do
            $logInfo
                ("Everyone is ready. \"" ++ sessionId ++ "\" was the last one.")
            sessionMap <- readTVarIO connectionMapTvar                
            gameTick <- gameTicks . gameState <$> readTVarIO gameLoopTvar
            broadcastMessage (GameWillStartMessage (GameStart (gameTick + 240))) sessionMap
            
            -- notify that the game will start
            atomically $ 
                writeTVar startGameTvar (Just (gameTick + 240))
            return ()

        return ()

broadcastMessage
    :: MonadIO m => ServerMessage -> Map Text ClientConnection -> m ()
broadcastMessage serverMessage sessionMap = forM_ sessionMap
    $ \conn -> atomically $ writeTChan (readChannel conn) serverMessage
