{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WaterWars.Server.EventLoop where

import           ClassyPrelude           hiding ( ask
                                                , Reader
                                                )
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Log
import qualified Control.Eff.Log               as EffLog
import           Control.Eff.Lift        hiding ( lift )

import           WaterWars.Network.Protocol
import           WaterWars.Core.Game
import           WaterWars.Server.ConnectionMgnt
import           WaterWars.Server.Env
import           WaterWars.Server.Events
import           WaterWars.Server.Action.Start
import           WaterWars.Server.Action.Restart
import           WaterWars.Server.Action.Util

eventLoop :: ('[Log Text, Reader Env] <:: r, MonadIO m, Lifted m r) => Eff r ()
eventLoop = forever $ do
    queue   <- reader (eventQueue . serverEnv)
    message <- atomically $ readTQueue queue
    EffLog.logE $ "Read a new event loop message: " ++ tshow message
    case message of
        -- handle messages sent by a client connection
        EventClientMessage sessionId clientMsg ->
            handleClientMessages sessionId clientMsg

        -- handle messages sent from the gameloop
        EventGameLoopMessage gameStateUpdate gameEvents ->
            handleGameLoopMessages gameStateUpdate gameEvents

        -- TODO: handle messages sent from websocket app

handleGameLoopMessages
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => GameState
    -> GameEvents
    -> Eff r ()
handleGameLoopMessages gameStateUpdate gameEvents = do
    ServerEnv {..}  <- reader serverEnv
    GameEnv {..}    <- reader gameEnv
    NetworkEnv {..} <- reader networkEnv

    let gameTick = gameTicks gameStateUpdate
    broadcastMessage (GameStateMessage gameStateUpdate gameEvents)
    gameIsRunning <- gameRunning <$> readTVarIO gameLoopTvar

    -- check if someone has won the game
    when
            (  gameIsRunning
            && length (getInGamePlayers $ inGamePlayers gameStateUpdate)
            <= 1
            )
        $ do
              EffLog.logE ("Restarting the game" :: Text)
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
            EffLog.logE ("An event is being executed" :: Text)
            futureToAction event



handleClientMessages
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Text
    -> ClientMessage
    -> Eff r ()
handleClientMessages sessionId clientMsg = case clientMsg of
    LoginMessage     login  -> loginMessage sessionId login

    LogoutMessage    logout -> logoutMessage sessionId logout

    GameSetupMessage setup  -> gameSetupMessage sessionId setup

    PlayerActionMessage playerAction ->
        playerActionMessage sessionId playerAction

    ClientReadyMessage clientReady -> clientReadyMessage sessionId clientReady

futureToAction
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => FutureEvent
    -> Eff r ()
futureToAction ResetGame = restartGameCallback
futureToAction StartGame = startGameCallback

loginMessage
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Text
    -> Login
    -> Eff r ()
loginMessage sessionId _ = do
    ServerEnv {..}  <- reader serverEnv
    GameEnv {..}    <- reader gameEnv
    NetworkEnv {..} <- reader networkEnv
            -- TODO: Handle reconnects
    EffLog.logE ("Login message from \"" ++ sessionId ++ "\"")
    -- Create new Player
    let player = newInGamePlayer (Player sessionId) (Location (0, 0))
    -- Tell the game loop engine about the newly connected player
    (connectionMay, gameMap_) <- atomically $ do
        serverState <- readTVar gameLoopTvar
        let serverState' = modifyGameState addInGamePlayer serverState player
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

logoutMessage
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Text
    -> Logout
    -> Eff r ()
logoutMessage sessionId Logout = do
    ServerEnv {..}  <- reader serverEnv
    GameEnv {..}    <- reader gameEnv
    NetworkEnv {..} <- reader networkEnv
    EffLog.logE ("Logout message from \"" ++ sessionId ++ "\"")
    playerMay <- lookup sessionId <$> readTVarIO playerMapTvar
    case playerMay of
        Nothing     -> return ()
        Just player -> atomically $ do
            serverState <- readTVar gameLoopTvar
            let serverState' = modifyGameState removePlayer
                                               serverState
                                               (playerDescription player)
            writeTVar gameLoopTvar serverState'
            modifyTVar' connectionMapTvar (deleteMap sessionId)
            modifyTVar' playerMapTvar     (deleteMap sessionId)
            modifyTVar' readyPlayersTvar  (deleteSet sessionId)
    return ()

gameSetupMessage
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Text
    -> GameSetup
    -> Eff r ()
gameSetupMessage _ _ = return ()

playerActionMessage
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Text
    -> PlayerAction
    -> Eff r ()
playerActionMessage sessionId playerAction = do
    ServerEnv {..}  <- reader serverEnv
    GameEnv {..}    <- reader gameEnv
    NetworkEnv {..} <- reader networkEnv
    playerMay       <- lookup sessionId <$> readTVarIO playerMapTvar
    case playerMay of
        Nothing -> do
            EffLog.logE
                ("Received a message that did not belong to any player" :: Text)
            return ()
        Just InGamePlayer {..} -> atomically $ do
            PlayerActions {..} <- readTVar playerActionTvar
            let getPlayerActions' = insertWith (++)
                                               playerDescription
                                               (getAction playerAction)
                                               getPlayerActions
            writeTVar playerActionTvar (PlayerActions getPlayerActions')
    return ()

clientReadyMessage
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Text
    -> ClientReady
    -> Eff r ()
clientReadyMessage sessionId ClientReady = do
    ServerEnv {..}  <- reader serverEnv
    GameEnv {..}    <- reader gameEnv
    NetworkEnv {..} <- reader networkEnv
    EffLog.logE ("Player \"" ++ sessionId ++ "\" is ready")

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
        EffLog.logE
            ("Everyone is ready. \"" ++ sessionId ++ "\" was the last one.")
        gameTick <- gameTicks . gameState <$> readTVarIO gameLoopTvar
        -- notify that the game will start
        broadcastMessage (GameWillStartMessage (GameStart (gameTick + 240)))

        -- add callback to start the game
        atomically $ modifyTVar' eventMapTvar
                                 (insertMap (gameTick + 240) StartGame)
        return ()
