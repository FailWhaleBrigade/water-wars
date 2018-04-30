module Main where

import ClassyPrelude
import Network.WebSockets

import System.Log.Logger
import System.Log.Handler.Simple

import WaterWars.Core.DefaultGame

import WaterWars.Network.Protocol as Protocol

import WaterWars.Server.Config
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameLoop

defaultState :: ServerState
defaultState = ServerState
    { connections = defaultConnections
    , gameMap     = defaultGameMap
    , gameState   = defaultGameState
    }

defaultConnections :: Connections
defaultConnections =
    Connections {players = mapFromList empty, gameSetup = Just defaultGameSetup}

defaultGameSetup :: GameSetup
defaultGameSetup =
    GameSetup {numberOfPlayers = 4, terrainMap = "default" -- TODO: not used for now
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


websocketServer :: MonadIO m => TVar ServerState -> TChan (Maybe PlayerAction) -> m ()
websocketServer serverStateTvar broadcastChan = liftIO $ runServer "localhost" 1234 $ \conn -> do
    connHandle <- acceptRequest conn
    debugM networkLoggerName "Client connected"
    commChan <- newTChanIO
    sendTextData connHandle (tshow $ Map defaultGameMap)
    atomically $ do
        serverState <- readTVar serverStateTvar
        writeTVar serverStateTvar (addChannel serverState commChan)

    clientGameThread connHandle broadcastChan commChan
    -- ! Should be used for cleanup code
    return ()

gameLoopServer :: MonadIO m => TVar ServerState -> TChan (Maybe PlayerAction) -> m ()
gameLoopServer serverStateTvar broadcastChan = do
    liftIO $ debugM networkLoggerName "Start listening on Gameloop"
    readBroadcastChan <- atomically $ dupTChan broadcastChan
    liftIO $ debugM networkLoggerName "Start game loop"
    runGameLoop serverStateTvar broadcastChan readBroadcastChan


