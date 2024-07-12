{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           ClassyPrelude           hiding ( Reader )

import           Effectful                    ( )
import           Effectful.Log

import           Data.UUID               hiding ( null )
import           Data.UUID.V4

import           Network.WebSockets      hiding ( newClientConnection )
import           Options.Applicative

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
import           System.Exit

serverStateWithGameMap :: GameMap -> GameLoopState
serverStateWithGameMap gameMap =
    GameLoopState {gameMap = gameMap, gameState = defaultGameState}

main :: IO ()
main = do
    Arguments {..} <- execParser opts
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
    let -- gameMapFiles_ :: [FilePath]
        gameMapFiles_ = fromList $ if null (gameMapFiles arguments)
            then ["resources/game1.txt"]
            else gameMapFiles arguments
    -- read resources
    -- TODO: this fails ugly
    terrains_ <- (mapM readTerrainFromFile gameMapFiles_)
    case sequenceA terrains_ of
        Nothing -> liftIO $ exitWith (ExitFailure 2)
        Just terrains -> do
            let loadedGameMaps = map (`GameMap` defaultDecoration) terrains
            -- Initialize server state
            messageQueue <- newTQueueIO
            -- start to accept connections
            _            <- async (websocketServer arguments messageQueue)
            gameServer arguments loadedGameMaps messageQueue


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
    atomically $ writeTQueue messageQueue (RegisterEvent (Player sessionId) conn)
    logger <- stdoutDateTextLogger
    clientGameThread
            logger
            conn
            (atomically . writeTQueue messageQueue . ClientMessageEvent
                (Player sessionId)
            )
            (atomically $ readTQueue commChan)
        `finally` ( atomically
                  . writeTQueue messageQueue
                  . ClientMessageEvent (Player sessionId)
                  $ LogoutMessage Logout
                  )
    return ()

gameServer
    :: MonadUnliftIO m
    => Arguments
    -> Seq GameMap
    -> TQueue EventMessage
    -> m ()
gameServer arguments loadedGameMaps messageQueue = do
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
    logger <- liftIO stdoutDateTextLogger
    liftIO $ race_ (runEventLoop logger envTvar messageQueue)
                        (runGameLoop envTvar messageQueue)

    -- $logInfo "Start game loop"
    return ()

stdoutDateTextLogger :: IO Logger
stdoutDateTextLogger = do
    mkLogger "server" (say . showLogMessage Nothing)
