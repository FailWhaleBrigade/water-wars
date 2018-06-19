{-# LANGUAGE TemplateHaskell #-}

module Main where

import ClassyPrelude
import Network.WebSockets hiding (newClientConnection)

import Control.Monad.Logger

import Data.UUID
import Data.UUID.V4

import WaterWars.Core.DefaultGame
import WaterWars.Core.Game
import WaterWars.Core.Terrain.Read

import WaterWars.Network.Protocol    as Protocol

import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameLoop
import WaterWars.Server.Client
import WaterWars.Server.EventLoop

defaultGameSetup :: GameSetup
defaultGameSetup = GameSetup {numberOfPlayers = 4, terrainMap = "default"}

serverStateWithTerrain :: Terrain -> GameLoopState
serverStateWithTerrain terrain = GameLoopState
    { gameMap     = GameMap
        { gameTerrain       = terrain
        , terrainDecoration = defaultDecoration
        }
    , gameState   = defaultGameState
    , gameRunning = False
    }

main :: IO ()
main = runLoop

runLoop :: (MonadIO m, MonadUnliftIO m) => m ()
runLoop = do
    -- read resources
    terrain           <- readTerrainFromFile "resources/game1.txt"

    -- Initialize server state
    broadcastChan     <- atomically newBroadcastTChan
    gameLoopStateTvar <- newTVarIO $ serverStateWithTerrain terrain
    sessionMapTvar    <- newTVarIO $ mapFromList []
    -- start to accept connections
    _                 <- async (websocketServer sessionMapTvar broadcastChan)
    forever
        ( runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ gameLoopServer gameLoopStateTvar sessionMapTvar broadcastChan
        )


websocketServer
    :: (MonadIO m, MonadUnliftIO m)
    => TVar (Map Text ClientConnection)
    -> TChan EventMessage
    -> m ()
websocketServer sessionMapTvar broadcastChan = liftIO $ runServer
    "localhost"
    1234
    (handleConnection sessionMapTvar broadcastChan)

handleConnection
    :: TVar (Map Text ClientConnection)
    -> TChan EventMessage
    -> PendingConnection
    -> IO ()
handleConnection sessionMapTvar broadcastChan websocketConn = do
    connHandle <- acceptRequest websocketConn
    commChan   <- newTChanIO -- to receive messages
    sessionId  <- toText <$> nextRandom -- uniquely identify connections
    let conn = newClientConnection sessionId connHandle commChan broadcastChan
    atomically $ modifyTVar' sessionMapTvar (insertMap sessionId conn)
    runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ clientGameThread
              conn
              ( atomically
              . writeTChan broadcastChan
              . EventClientMessage sessionId
              )
              (atomically $ readTChan commChan)
    -- ! Should be used for cleanup code
    return ()

gameLoopServer
    :: (MonadIO m, MonadLogger m, MonadUnliftIO m)
    => TVar GameLoopState
    -> TVar (Map Text ClientConnection)
    -> TChan EventMessage
    -> m ()
gameLoopServer gameLoopStateTvar sessionMapTvar broadcastChan = do
    readBroadcastChan <- atomically $ dupTChan broadcastChan
    playerActionTvar  <- newTVarIO (PlayerActions (mapFromList empty))
    playerInGameTvar  <- newTVarIO $ mapFromList []
    readyPlayersTvar  <- newTVarIO mempty

    _                 <- async
        (eventLoop readBroadcastChan
                   gameLoopStateTvar
                   playerActionTvar
                   sessionMapTvar
                   playerInGameTvar
                   readyPlayersTvar
        )
    $logInfo "Start game loop"
    runGameLoop gameLoopStateTvar broadcastChan playerActionTvar
    return ()
