{-# LANGUAGE LambdaCase #-}

module Main where

import           ClassyPrelude
import           Network.WebSockets

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
import           System.IO                                ( openFile )
import           Data.Array.IArray
import           Data.List                                ( transpose )

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
defaultGameSetup = GameSetup {numberOfPlayers = 4, terrainMap = "default"}

serverStateWithTerrain :: Terrain -> ServerState
serverStateWithTerrain terrain = ServerState
    { connections = defaultConnections
    , gameMap     = GameMap
        { gameTerrain       = terrain
        , gamePlayers       = empty
        , terrainBackground = "default"
        }
    , gameState   = defaultGameState
    }

-- TODO: refactor startup

main :: IO ()
main = do
  -- Copy everything to syslog from here on out.
    s <- fileHandler "water-wars-server.log" DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)
    updateGlobalLogger rootLoggerName (setLevel DEBUG)

    -- read resources
    terrainFile           <- openFile "resources/game1.txt" ReadMode
    content :: ByteString <- hGetContents terrainFile
    let terrain =
            Terrain
                $ listArray (BlockLocation (-8, -8), BlockLocation (8, 8))
                . map (\case
                    'x' -> SolidBlock Middle
                    '_' -> SolidBlock Ceil
                    '-' -> SolidBlock Floor
                    _ -> NoBlock
                    )
                . concat
                . transpose
                . reverse
                . lines
                . unpack
                . decodeUtf8
                $ content

    -- Initialize server state
    broadcastChan   <- atomically newBroadcastTChan
    serverStateTvar <- newTVarIO $ serverStateWithTerrain terrain

    -- start to accept connections
    _               <- async (websocketServer serverStateTvar broadcastChan)
    _               <- forever (gameLoopServer serverStateTvar broadcastChan)
    return ()


websocketServer
    :: MonadIO m => TVar ServerState -> TChan (ClientMessage, Text) -> m ()
websocketServer serverStateTvar broadcastChan =
    liftIO
        $ runServer "localhost" 1234
        $ \websocketConn -> do
              connHandle <- acceptRequest websocketConn
              debugM networkLoggerName "Client connected"
              commChan  <- newTChanIO -- to receive messages
              sessionId <- toText <$> nextRandom -- uniquely identify connections
              let
                  conn = newClientConnection sessionId
                                             connHandle
                                             commChan
                                             broadcastChan
              atomically $ do
                  ServerState {..} <- readTVar serverStateTvar
                  writeTVar
                      serverStateTvar
                      ServerState
                          { connections = addConnection connections conn
                          , ..
                          }
              clientGameThread conn
              -- ! Should be used for cleanup code
              return ()

gameLoopServer
    :: MonadIO m => TVar ServerState -> TChan (ClientMessage, Text) -> m ()
gameLoopServer serverStateTvar broadcastChan = do
    readBroadcastChan <- atomically $ dupTChan broadcastChan
    playerActionTvar  <- newTVarIO (PlayerActions (mapFromList empty))
    _                 <- liftIO . async $ eventLoop readBroadcastChan
                                                    serverStateTvar
                                                    playerActionTvar
                                                    (mapFromList empty)
    liftIO $ debugM networkLoggerName "Start game loop"
    runGameLoop serverStateTvar playerActionTvar
