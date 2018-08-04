{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Server.EventLoop where

import ClassyPrelude

import Control.Monad.Logger

import WaterWars.Network.Protocol
import WaterWars.Core.Game
import WaterWars.Server.State

eventLoop :: (MonadIO m, MonadLogger m) => SharedState -> m ()
eventLoop sharedState@SharedState {..} = forever $ do
    message <- atomically $ readTQueue eventQueue
    $logDebug $ "Read a new event loop message: " ++ tshow message
    case message of
        -- handle messages sent by a client connection
        EventClientMessage sessionId clientMsg ->
            handleClientMessages sharedState sessionId clientMsg

        -- handle messages sent from the gameloop
        EventGameLoopMessage gameStateUpdate gameEvents ->
            handleGameLoopMessages sharedState gameStateUpdate gameEvents

        -- TODO: handle messages sent from websocket app

handleGameLoopMessages
    :: (MonadIO m, MonadLogger m)
    => SharedState
    -> GameState
    -> GameEvents
    -> m ()
handleGameLoopMessages SharedState {..} gameStateUpdate gameEvents = do
    sessionMap <- readTVarIO connectionMapTvar
    let gameTick = gameTicks gameStateUpdate
    broadcastMessage (GameStateMessage gameStateUpdate gameEvents) sessionMap
    gameIsRunning <- gameRunning <$> readTVarIO gameLoopTvar

    -- check if someone has won the game
    when
            (  gameIsRunning
            && length (getInGamePlayers $ inGamePlayers gameStateUpdate)
            <= 1
            )
        $ do
              $logInfo "Restarting the game"
              atomically $ do
                  modifyTVar' gameLoopTvar stopGame
                  modifyTVar' eventMapTvar
                              (insertMap (gameTick + 240) ResetGame)

    eventMay <- atomically $ do
        eventMap <- readTVar eventMapTvar
        case lookup gameTick eventMap of
            Nothing    -> return Nothing
            Just event -> do
                modifyTVar' eventMapTvar (deleteMap gameTick)
                return $ Just event

    case eventMay of
        Nothing    -> return ()
        Just event -> do
            $logInfo "An event is being executed"
            futureToAction event SharedState {..}



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
                writeTQueue
                    (readChannel connection)
                    (LoginResponseMessage (LoginResponse sessionId player))
                writeTQueue (readChannel connection) (GameMapMessage gameMap_)
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
                    return $ keysSet playerMap == readySet'

        when allPlayersReady $ do
            $logInfo
                ("Everyone is ready. \"" ++ sessionId ++ "\" was the last one.")
            sessionMap <- readTVarIO connectionMapTvar
            gameTick   <- gameTicks . gameState <$> readTVarIO gameLoopTvar
            -- notify that the game will start
            broadcastMessage
                (GameWillStartMessage (GameStart (gameTick + 240)))
                sessionMap
            -- add callbakc to start the game
            atomically $ modifyTVar' eventMapTvar
                                     (insertMap (gameTick + 240) StartGame)
            return ()

        return ()

broadcastMessage
    :: MonadIO m => ServerMessage -> Map Text ClientConnection -> m ()
broadcastMessage serverMessage sessionMap = forM_ sessionMap
    $ \conn -> atomically $ writeTQueue (readChannel conn) serverMessage

futureToAction
    :: (MonadLogger m, MonadIO m) => FutureEvent -> SharedState -> m ()
futureToAction ResetGame = restartGameCallback
futureToAction StartGame = startGameCallback

startGameCallback :: (MonadLogger m, MonadIO m) => SharedState -> m ()
startGameCallback SharedState {..} = do
    gameTick <- gameTicks . gameState <$> readTVarIO gameLoopTvar
    $logInfo $ "Send the Game start message: " ++ tshow gameTick
    atomically $ modifyTVar' gameLoopTvar startGame

    sessionMap <- readTVarIO connectionMapTvar
    broadcastMessage GameStartMessage sessionMap

restartGameCallback :: (MonadLogger m, MonadIO m) => SharedState -> m ()
restartGameCallback SharedState {..} = do
    $logInfo $ "Restart the game"
    (sessionMap, newGameMap_) <- atomically $ do
        writeTVar readyPlayersTvar mempty -- demand that everyone ready's up again
        sessionMap  <- readTVar connectionMapTvar
        nextGameMap_ <- nextGameMap gameMapTvar

        modifyTVar' gameLoopTvar $ \gameLoop ->
            let
                GameState {..} = gameState gameLoop
                -- add all dead players to alive ones
                inGamePlayers' =
                    InGamePlayers
                        $  map (\p -> p { playerLocation = Location (0, 0) })
                               (getInGamePlayers inGamePlayers)
                        ++ map
                               (\DeadPlayer {..} -> newInGamePlayer
                                   deadPlayerDescription
                                   (Location (0, 0))
                               )
                               (getDeadPlayers gameDeadPlayers)

                stoppedGameLoop = gameLoop
                    { gameState   = GameState
                        { inGamePlayers   = inGamePlayers'
                        , gameDeadPlayers = DeadPlayers empty
                        , ..
                        }
                    , gameRunning = False
                    , gameMap     = nextGameMap_
                    }
            in
                stoppedGameLoop

        return (sessionMap, nextGameMap_)

    broadcastMessage ResetGameMessage            sessionMap
    broadcastMessage (GameMapMessage newGameMap_) sessionMap


