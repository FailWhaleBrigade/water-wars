module WaterWars.Server.ConnectionMgnt where

import ClassyPrelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class
import System.Log.Logger

import Network.WebSockets

import WaterWars.Network.Protocol

import WaterWars.Core.GameState

import WaterWars.Server.Config

data ServerState = ServerState
    { connections :: Connections
    , gameMap     :: GameMap
    , gameState   :: GameState
    }

data Connections = Connections
    { players :: Map Player (TChan GameInformation)
    , gameSetup :: Maybe GameSetup
    }

addChannel :: ServerState -> TChan GameInformation -> ServerState
addChannel ServerState {..} chan =
    let Connections {..} = connections
        newPlayer        = Player $ tshow $ length players
    in  ServerState
            { connections = connections
                { players = insertMap newPlayer chan players
                }
            , ..
            } -- TODO: temporary solution, please fix!

broadcastGameState :: MonadIO m => Connections -> GameState -> m ()
broadcastGameState Connections {..} state =
    forM_ players $ \chan -> atomically $ writeTChan chan (State state)

clientGameThread
    :: MonadIO m
    => Connection --  ^Connection of the client
    -> TChan PlayerAction -- ^Broadcast channel to send all messages to
    -> TChan GameInformation -- ^Information channel that sends messages to client
    -> m ()
clientGameThread conn broadcastChan receiveChan = liftIO $ do
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions 
    _ <- async (clientReceive conn broadcastChan)
    clientSend conn receiveChan


clientReceive
    :: MonadIO m
    => Connection   --  ^Connection of the client
    -> TChan PlayerAction  -- ^Broadcast channel to send all messages to
    -> m ()
clientReceive conn broadcastChan = forever $ do
    liftIO $ debugM "Server.Connection" "Wait for data message"
    msg :: Text <- liftIO $ receiveData conn
    case readMay msg of
        Nothing -> do
            liftIO $ infoM networkLoggerName "Could not read message"
            liftIO $ debugM networkLoggerName
                            (show $ "Could not read message: " ++ msg)
        Just playerAction -> atomically $ writeTChan broadcastChan playerAction
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes
    return ()

clientSend
    :: MonadIO m
    => Connection  --  ^Connection of the client 
    -> TChan GameInformation -- ^Information channel that sends messages to client
    -> m ()
clientSend conn sendChan = forever $ do
    liftIO $ debugM "Server.Connection" "Wait for message"
    cmd <- liftIO . atomically $ readTChan sendChan
    liftIO $ sendTextData conn $ tshow cmd
    -- TODO: sleep for fun?
    return ()