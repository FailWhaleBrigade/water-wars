{-# LANGUAGE TemplateHaskell #-}

module Main where

import ClassyPrelude
import Network.WebSockets hiding (newClientConnection)

import Control.Monad.Logger

import Data.UUID
import Data.UUID.V4

import Options.Applicative

import WaterWars.Core.DefaultGame
import WaterWars.Core.Game
import WaterWars.Core.Terrain.Read

import WaterWars.Network.Protocol    as Protocol

import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameLoop
import WaterWars.Server.Client
import WaterWars.Server.EventLoop
import WaterWars.Server.State
import WaterWars.Server.OptParse

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
main = execParser opts >>= runLoop
  where
    opts = info
        (argumentsParser <**> helper)
        (  fullDesc
        <> progDesc "Start an instance of the water-wars server."
        <> header "Fail Whale Brigade presents Water Wars."
        )

runLoop :: (MonadIO m, MonadUnliftIO m) => Arguments -> m ()
runLoop arguments = do
    -- read resources
    terrain           <- readTerrainFromFile "resources/game1.txt"

    -- Initialize server state
    broadcastChan     <- atomically newBroadcastTChan
    gameLoopStateTvar <- newTVarIO $ serverStateWithTerrain terrain
    sessionMapTvar    <- newTVarIO $ mapFromList []
    -- start to accept connections
    _ <- async (websocketServer arguments sessionMapTvar broadcastChan)
    forever
        ( runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ gameLoopServer gameLoopStateTvar sessionMapTvar broadcastChan
        )


websocketServer
    :: (MonadIO m, MonadUnliftIO m)
    => Arguments
    -> TVar (Map Text ClientConnection)
    -> TChan EventMessage
    -> m ()
websocketServer Arguments {..} sessionMapTvar broadcastChan =
    liftIO $ runServer (unpack hostname)
                       port
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
        $         filterLogger (\_ level -> level /= LevelDebug)
        $         clientGameThread
                      conn
                      ( atomically
                      . writeTChan broadcastChan
                      . EventClientMessage sessionId
                      )
                      (atomically $ readTChan commChan)
        `finally` ( atomically
                  . writeTChan broadcastChan
                  . EventClientMessage sessionId
                  $ LogoutMessage Logout
                  )
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
    gameStartTvar     <- newTVarIO Nothing
    let sharedState = SharedState readBroadcastChan
                                  gameLoopStateTvar
                                  playerActionTvar
                                  sessionMapTvar
                                  playerInGameTvar
                                  readyPlayersTvar
                                  gameStartTvar
    _ <- async (eventLoop sharedState)
    $logInfo "Start game loop"
    runGameLoop gameLoopStateTvar broadcastChan playerActionTvar
    return ()


