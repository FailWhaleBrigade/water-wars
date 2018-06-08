module Main where

import           ClassyPrelude
import           Network.WebSockets hiding (newClientConnection)

import           System.Log.Logger
import           System.Log.Handler.Simple

import           Data.UUID
import           Data.UUID.V4

import           WaterWars.Core.DefaultGame
import           WaterWars.Core.Game

import           WaterWars.Network.Protocol    as Protocol

import           WaterWars.Server.Config
import           WaterWars.Server.ConnectionMgnt
import           WaterWars.Server.GameLoop
import           WaterWars.Server.Client
import           WaterWars.Server.EventLoop
import           WaterWars.Core.Terrain.Read

defaultState :: GameLoopState
defaultState =
    GameLoopState {gameMap = defaultGameMap, gameState = defaultGameState}

defaultGameSetup :: GameSetup
defaultGameSetup = GameSetup {numberOfPlayers = 4, terrainMap = "default"}

serverStateWithTerrain :: Terrain -> GameLoopState
serverStateWithTerrain terrain = GameLoopState
    { gameMap   = GameMap
        { gameTerrain       = terrain
        , gamePlayers       = empty
        , terrainBackground = "default"
        }
    , gameState = defaultGameState
    }

main :: IO ()
main = do
  -- Copy everything to syslog from here on out.
    s <- fileHandler "water-wars-server.log" DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)
    updateGlobalLogger rootLoggerName (setLevel DEBUG)

    -- read resources
    terrain           <- readTerrainFromFile "resources/game1.txt"

    -- Initialize server state
    broadcastChan     <- atomically newBroadcastTChan
    gameLoopStateTvar <- newTVarIO $ serverStateWithTerrain terrain
    sessionMapTvar    <- newTVarIO $ mapFromList []
    -- start to accept connections
    _                 <- async (websocketServer sessionMapTvar broadcastChan)
    forever (gameLoopServer gameLoopStateTvar sessionMapTvar broadcastChan)


websocketServer
    :: MonadIO m
    => TVar (Map Text ClientConnection)
    -> TChan EventMessage
    -> m ()
websocketServer sessionMapTvar broadcastChan =
    liftIO $ runServer "localhost" 1234 $ \websocketConn -> do
        connHandle <- acceptRequest websocketConn
        debugM networkLoggerName "Client connected"
        commChan  <- newTChanIO -- to receive messages
        sessionId <- toText <$> nextRandom -- uniquely identify connections
        let conn = newClientConnection sessionId
                                       connHandle
                                       commChan
                                       broadcastChan
        atomically $ modifyTVar' sessionMapTvar (insertMap sessionId conn)
        clientGameThread
            conn
            (\msg -> atomically $ writeTChan
                broadcastChan
                (EventClientMessage sessionId msg)
            )
            (atomically $ readTChan commChan)
        -- ! Should be used for cleanup code
        return ()

gameLoopServer
    :: MonadIO m
    => TVar GameLoopState
    -> TVar (Map Text ClientConnection)
    -> TChan EventMessage
    -> m ()
gameLoopServer gameLoopStateTvar sessionMapTvar broadcastChan = do
    readBroadcastChan <- atomically $ dupTChan broadcastChan
    playerActionTvar  <- newTVarIO (PlayerActions (mapFromList empty))
    playerInGameTVar  <- newTVarIO $ mapFromList []
    _                 <- liftIO . async $ eventLoop readBroadcastChan
                                                    gameLoopStateTvar
                                                    playerActionTvar
                                                    sessionMapTvar
                                                    playerInGameTVar
    liftIO $ debugM networkLoggerName "Start game loop"
    runGameLoop gameLoopStateTvar broadcastChan playerActionTvar
