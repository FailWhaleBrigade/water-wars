module Main where

import ClassyPrelude
import Network.WebSockets

import System.Log.Logger
import System.Log.Handler.Simple

import WaterWars.Core.DefaultGame
import WaterWars.Core.GameState

import WaterWars.Network.Protocol as Protocol

import WaterWars.Server.Config
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameLoop
import WaterWars.Server.Client
import WaterWars.Server.EventLoop

defaultState :: ServerState
defaultState = ServerState
    { connections = defaultConnections
    , gameMap     = defaultGameMap
    , gameState   = defaultGameState
    }

defaultConnections :: Connections ServerMessage ClientMessage
defaultConnections =
    Connections {players = mapFromList empty, gameSetup = Just defaultGameSetup }

defaultGameSetup :: GameSetup
defaultGameSetup =
    GameSetup {numberOfPlayers = 4, terrainMap = "default" 
                                                          }

main :: IO ()
main = do
  -- Copy everything to syslog from here on out.
    s <- fileHandler "water-wars-server.log" DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)
    updateGlobalLogger rootLoggerName (setLevel DEBUG)

    -- Initialize server state
    broadcastChan   <- atomically newBroadcastTChan
    serverStateTvar <- newTVarIO defaultState

    -- start to accept connections
    _ <- async (websocketServer serverStateTvar broadcastChan)
    _ <- forever (gameLoopServer serverStateTvar broadcastChan)
    return ()


websocketServer :: MonadIO m => TVar ServerState -> TChan (ClientMessage, Player) -> m ()
websocketServer serverStateTvar broadcastChan = liftIO $ runServer "localhost" 1234 $ \websocketConn -> do
    connHandle <- acceptRequest websocketConn
    debugM networkLoggerName "Client connected"
    commChan <- newTChanIO
    sendTextData connHandle (tshow $ GameMapMessage defaultGameMap)
    let player = Player "Player One"
    let conn = newClientConnection "Player One" player connHandle commChan broadcastChan :: ClientConnection ServerMessage ClientMessage
    atomically $ do
        ServerState {..} <- readTVar serverStateTvar
        let newConnections = addConnection connections conn
        writeTVar serverStateTvar ServerState {connections = newConnections, ..}
    clientGameThread conn
    -- ! Should be used for cleanup code
    return ()

gameLoopServer :: MonadIO m => TVar ServerState -> TChan (ClientMessage, Player) -> m ()
gameLoopServer serverStateTvar broadcastChan = do
    readBroadcastChan <- atomically $ dupTChan broadcastChan
    tmvar <- newTMVarIO (PlayerActions (mapFromList []))
    _ <- liftIO . async $ eventLoop readBroadcastChan tmvar
    liftIO $ debugM networkLoggerName "Start game loop"
    runGameLoop serverStateTvar tmvar


