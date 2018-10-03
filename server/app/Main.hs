{-# LANGUAGE DataKinds #-}

module Main where

import           ClassyPrelude           hiding ( Reader )

import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Eff                    ( )
import           Control.Eff.Lift
import           Control.Eff.Log                ( runLog
                                                , Logger
                                                )
import           Control.Eff.Reader.Strict

import           Data.UUID               hiding ( null )
import           Data.UUID.V4
import           Data.Time

import           Network.WebSockets      hiding ( newClientConnection )
import           Options.Applicative

import           System.Remote.Monitoring

import           WaterWars.Core.DefaultGame
import           WaterWars.Core.Game
import           WaterWars.Core.Terrain.Read

import           WaterWars.Network.Protocol    as Protocol

import           WaterWars.Server.ConnectionMgnt
import           WaterWars.Server.GameLoop
import           WaterWars.Server.ClientConnection
import           WaterWars.Server.EventLoop
import           WaterWars.Server.Env
import           OptParse

defaultGameSetup :: GameSetup
defaultGameSetup = GameSetup {numberOfPlayers = 4, terrainMap = "default"}

serverStateWithGameMap :: GameMap -> GameLoopState
serverStateWithGameMap gameMap = GameLoopState
    { gameMap     = gameMap
    , gameState   = defaultGameState
    , gameRunning = False
    }

main :: IO ()
main = do
    Arguments {..} <- execParser opts
    _              <- forkServer "0.0.0.0" monitorPort
    runLoop Arguments {..}
  where
    opts = info
        (argumentsParser <**> helper)
        (  fullDesc
        <> progDesc "Start an instance of the water-wars server."
        <> header "Fail Whale Brigade presents Water Wars."
        )

runLoop :: (MonadIO m, MonadUnliftIO m) => Arguments -> m ()
runLoop arguments = do
    let gameMapFiles_ = fromList $ if null (gameMapFiles arguments)
            then ["resources/game1.txt"]
            else gameMapFiles arguments
    -- read resources
    -- TODO: this fails ugly
    terrains <- mapM readTerrainFromFile gameMapFiles_
    let loadedGameMaps = map (`GameMap` defaultDecoration) terrains
    -- Initialize server state
    broadcastChan     <- newTQueueIO
    gameLoopStateTvar <- newTVarIO
        $ serverStateWithGameMap (headEx loadedGameMaps)
    sessionMapTvar <- newTVarIO $ mapFromList []
    -- start to accept connections
    _ <- async (websocketServer arguments sessionMapTvar broadcastChan)
    forever
        (gameLoopServer arguments
                        loadedGameMaps
                        gameLoopStateTvar
                        sessionMapTvar
                        broadcastChan
        )


websocketServer
    :: (MonadIO m, MonadUnliftIO m)
    => Arguments
    -> TVar (Map Text ClientConnection)
    -> TQueue EventMessage
    -> m ()
websocketServer Arguments {..} sessionMapTvar broadcastChan =
    liftIO $ runServer (unpack hostname)
                       port
                       (handleConnection sessionMapTvar broadcastChan)

handleConnection
    :: TVar (Map Text ClientConnection)
    -> TQueue EventMessage
    -> PendingConnection
    -> IO ()
handleConnection sessionMapTvar broadcastChan websocketConn = do
    connHandle <- acceptRequest websocketConn
    commChan   <- newTQueueIO -- to receive messages
    sessionId  <- toText <$> nextRandom -- uniquely identify connections
    let conn = newClientConnection sessionId connHandle commChan broadcastChan
    atomically $ modifyTVar' sessionMapTvar (insertMap sessionId conn)
    clientGameThread
            stdoutDateTextLogger
            conn
            ( liftIO
            . atomically
            . writeTQueue broadcastChan
            . EventClientMessage sessionId
            )
            (atomically $ readTQueue commChan)
        `finally` ( atomically
                  . writeTQueue broadcastChan
                  . EventClientMessage sessionId
                  $ LogoutMessage Logout
                  )
    return ()

gameLoopServer
    :: (MonadIO m, MonadUnliftIO m)
    => Arguments
    -> Seq GameMap
    -> TVar GameLoopState
    -> TVar (Map Text ClientConnection)
    -> TQueue EventMessage
    -> m ()
gameLoopServer arguments loadedGameMaps gameLoopStateTvar sessionMapTvar broadcastChan
    = do
        playerActionTvar <- newTVarIO (PlayerActions (mapFromList empty))
        playerInGameTvar <- newTVarIO $ mapFromList []
        readyPlayersTvar <- newTVarIO mempty
        eventMapTvar     <- newTVarIO $ mapFromList []
        gameMapTvar      <- newTVarIO $ GameMaps loadedGameMaps 0
        let env :: Env = Env broadcastChan
                             gameLoopStateTvar
                             playerActionTvar
                             sessionMapTvar
                             playerInGameTvar
                             readyPlayersTvar
                             gameMapTvar
                             eventMapTvar
                             (fps arguments)

        _ <-
            liftIO
            $ async
            . runLift
            . runLog stdoutDateTextLogger
            . runReader env
            $ eventLoop

        -- $logInfo "Start game loop"
        runGameLoop env gameLoopStateTvar broadcastChan playerActionTvar
        return ()

stdoutDateTextLogger :: MonadBase IO m => Logger m Text
stdoutDateTextLogger msg = liftBase $ do
    time <- getCurrentTime
    let fmtTime = formatTime defaultTimeLocale rfc822DateFormat time
    say $ pack fmtTime ++ ": " ++ msg
