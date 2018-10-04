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
import           WaterWars.Server.Events
import           OptParse

serverStateWithGameMap :: GameMap -> GameLoopState
serverStateWithGameMap gameMap =
    GameLoopState {gameMap = gameMap, gameState = defaultGameState}

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

runLoop :: MonadUnliftIO m => Arguments -> m ()
runLoop arguments = do
    let gameMapFiles_ = fromList $ if null (gameMapFiles arguments)
            then ["resources/game1.txt"]
            else gameMapFiles arguments
    -- read resources
    -- TODO: this fails ugly
    terrains <- mapM readTerrainFromFile gameMapFiles_
    let loadedGameMaps = map (`GameMap` defaultDecoration) terrains
    -- Initialize server state
    messageQueue <- newTQueueIO
    -- start to accept connections
    _            <- async (websocketServer arguments messageQueue)
    forever (gameLoopServer arguments loadedGameMaps messageQueue)


websocketServer :: MonadUnliftIO m => Arguments -> TQueue EventMessage -> m ()
websocketServer Arguments {..} messageQueue =
    liftIO $ runServer (unpack hostname) port (handleConnection messageQueue)

handleConnection :: TQueue EventMessage -> PendingConnection -> IO ()
handleConnection messageQueue websocketConn = do
    connHandle <- acceptRequest websocketConn
    commChan   <- newTQueueIO -- to receive messages
    sessionId  <- toText <$> nextRandom -- uniquely identify connections
    let conn = newClientConnection sessionId
                                   connHandle
                                   (commChan :: TQueue ServerMessage)
                                   (messageQueue :: TQueue EventMessage)
    atomically $ writeTQueue messageQueue (Register sessionId conn)
    clientGameThread
            stdoutDateTextLogger
            conn
            ( atomically
            . writeTQueue messageQueue
            . EventClientMessage sessionId
            )
            (atomically $ readTQueue commChan)
        `finally` ( atomically
                  . writeTQueue messageQueue
                  . EventClientMessage sessionId
                  $ LogoutMessage Logout
                  )
    return ()

gameLoopServer
    :: MonadUnliftIO m
    => Arguments
    -> Seq GameMap
    -> TQueue EventMessage
    -> m ()
gameLoopServer arguments loadedGameMaps messageQueue = do
    let gameLoopState = serverStateWithGameMap (headEx loadedGameMaps)
    let playerAction  = PlayerActions (mapFromList empty)
    let playerInGame  = mapFromList []
    let readyPlayers  = mempty
    let eventMap      = mapFromList []
    let gameMap       = GameMaps loadedGameMaps 0
    let serverState   = WarmUp

    let networkEnv    = NetworkEnv {connectionMap = mempty}
    let gameEnv = GameEnv
            { playerMap    = playerInGame
            , readyPlayers = readyPlayers
            , playerAction = playerAction
            }
    let gameConfig = GameConfig {fps = gameFps arguments, gameMaps = gameMap}
    let serverEnv = ServerEnv
            { gameLoop    = gameLoopState
            , eventMap    = eventMap
            , serverState = serverState
            }
    let env :: Env = Env {..}
    envTvar :: TVar Env <- newTVarIO env
    let logger :: Logger IO Text = stdoutDateTextLogger
    _ <- liftIO $async $ runEventLoop logger envTvar messageQueue

    -- $logInfo "Start game loop"
    runGameLoop envTvar messageQueue
    return ()

stdoutDateTextLogger :: MonadBase IO m => Logger m Text
stdoutDateTextLogger msg = liftBase $ do
    time <- getCurrentTime
    let fmtTime = formatTime defaultTimeLocale rfc822DateFormat time
    say $ pack fmtTime ++ ": " ++ msg
