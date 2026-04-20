{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (finally, Exception (..), catch, SomeException)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.UUID hiding (null)
import Data.UUID.V4
import Effectful (MonadIO (..))
import Effectful.Log
import Network.WebSockets hiding (newClientConnection)
import OptParse
import Options.Applicative
import Say
import System.Exit
import WaterWars.Core.DefaultGame
import WaterWars.Core.Game
import WaterWars.Core.Terrain.Read
import WaterWars.Network.Protocol as Protocol
import WaterWars.Server.ClientConnection
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.Env
import WaterWars.Server.EventLoop
import WaterWars.Server.Events
import WaterWars.Server.GameLoop
import Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai
import Data.Function
import Network.HTTP.Types.Status (notFound404)
import qualified Network.WebSockets as WS


serverStateWithGameMap :: GameMap -> GameLoopState
serverStateWithGameMap gameMap =
    GameLoopState {gameMap = gameMap, gameState = defaultGameState}

main :: IO ()
main = do
    args <- execParser opts
    startServer args
  where
    opts = info
        (argumentsParser <**> helper)
        (  fullDesc
        <> progDesc "Start an instance of the water-wars server."
        <> header "Fail Whale Brigade presents Water Wars."
        )

startServer :: Arguments -> IO ()
startServer arguments = do
    let -- gameMapFiles_ :: [FilePath]
        gameMapFiles_ = Seq.fromList $ if null (gameMapFiles arguments)
            then ["resources/game1.txt"]
            else gameMapFiles arguments
    -- read resources
    -- TODO: this fails ugly
    terrains_ <- (mapM readTerrainFromFile gameMapFiles_)
    case sequenceA terrains_ of
        Nothing -> exitWith (ExitFailure 2)
        Just terrains -> do
            let loadedGameMaps = fmap (`GameMap` defaultDecoration) terrains
            -- Initialize server state
            messageQueue <- newTQueueIO
            -- start to accept connections
            _ <- withAsync (websocketServer arguments messageQueue) $ \ wss -> do
                gameServer arguments loadedGameMaps messageQueue
                wait wss
            pure ()

websocketServer :: Arguments -> TQueue EventMessage -> IO ()
websocketServer Arguments {..} messageQueue = do
    -- WS.runServer "127.0.0.1" 8080 (handleConnection messageQueue)
    runSettings
        (defaultSettings
            -- & setHost (fromString $ Text.unpack hostname)
            & setPort port)
        (WS.websocketsOr defaultConnectionOptions (handleConnection messageQueue)
            (\ _request handler -> handler $ responseLBS notFound404 mempty ""))

handleConnection :: TQueue EventMessage -> PendingConnection -> IO ()
handleConnection messageQueue websocketConn = do
    putStrLn "Waiting for connection"
    connHandle <- acceptRequest websocketConn
    WS.withPingThread connHandle 30 (pure ()) $ do

        (clientHandler connHandle) `catch` \ e -> putStrLn (displayException @SomeException e)
    where
        clientHandler connHandle = do
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
                    (liftIO . atomically . writeTQueue messageQueue . ClientMessageEvent
                        (Player sessionId)
                    )
                    (liftIO $ atomically $ readTQueue commChan)
                `finally` ( atomically
                        . writeTQueue messageQueue
                        . ClientMessageEvent (Player sessionId)
                        $ LogoutMessage Logout
                        )

gameServer
    :: Arguments
    -> Seq GameMap
    -> TQueue EventMessage
    -> IO ()
gameServer arguments loadedGameMaps messageQueue = do
    let gameLoopState = serverStateWithGameMap (head $ Foldable.toList loadedGameMaps)
    let playerAction  = PlayerActions Map.empty
    let playerInGame  = Map.empty
    let readyPlayers  = Set.empty
    let eventMap      = Map.empty
    let gameMap       = GameMaps loadedGameMaps 0
    let serverState   = WarmUp

    let networkEnv    = NetworkEnv {connectionMap = Map.empty}
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
    envTvar :: TVar Env <- liftIO $ newTVarIO env
    logger <- liftIO stdoutDateTextLogger
    liftIO $ race_ (runEventLoop logger envTvar messageQueue)
                    (runGameLoop envTvar messageQueue)
    return ()

stdoutDateTextLogger :: IO Logger
stdoutDateTextLogger = do
    mkLogger "server" (say . showLogMessage Nothing)
