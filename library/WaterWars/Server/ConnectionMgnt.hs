{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module WaterWars.Server.ConnectionMgnt
    ( ServerState(..)
    , Connections(..)
    , PlayerActions(..)
    , ClientConnection(connectionId)
    , newClientConnection
    , addConnection
    , removeConnection
    , addPlayer
    , removePlayer
    , modifyGameState
    , modifyConnections
    ) where

import ClassyPrelude

import qualified Network.WebSockets as WS

import WaterWars.Network.Protocol
import WaterWars.Network.Connection

import WaterWars.Core.GameState
import WaterWars.Core.GameMap
import WaterWars.Core.GameAction

data ServerState = ServerState
    { connections :: Connections
    , gameMap     :: GameMap
    , gameState   :: GameState
    } deriving (Show, Eq)

newtype PlayerActions = PlayerActions
    { getPlayerActions :: Map Player Action
    } deriving (Show, Eq)

data Connections = Connections
    { players       :: Map Text ClientConnection
    , gameSetup     :: Maybe GameSetup
    } deriving (Show, Eq, Ord)

data ClientConnection = ClientConnection
    { connectionId  :: Text -- ^Session id, uniquely identifies players
    , connection    :: WS.Connection -- ^Abstraction over an connection handle
    , readChannel   :: TChan ServerMessage -- ^Client threads read from this channel
    , writeChannel  :: TChan (ClientMessage, Text) -- ^Client threads write to this channel, with its id
    }

instance Eq ClientConnection where
    c1 == c2 = connectionId c1 == connectionId c2

instance Ord ClientConnection where
    c1 <= c2 = connectionId c1 <= connectionId c2

instance Show ClientConnection where
    show ClientConnection {..} = "ClientConnection { connectionId = " ++ show connectionId ++ "}"

instance NetworkConnection ClientConnection where
    type SendType ClientConnection = ServerMessage
    type ReceiveType ClientConnection = ClientMessage
    send :: MonadIO m => ClientConnection -> ServerMessage -> m ()
    send conn toSend = do
        let msg = serialize toSend
        liftIO $ WS.sendTextData (connection conn) msg

    receive :: MonadIO m => ClientConnection -> m (Either Text ClientMessage)
    receive conn = do
        msg <- liftIO $ WS.receiveData (connection conn)
        case deserialize msg of
            Nothing -> return $ Left msg
            Just action -> return $ Right action

instance IPC ClientConnection where
    type Identifier ClientConnection = Text
    type WriteTo ClientConnection = ClientMessage
    type ReadFrom ClientConnection = ServerMessage

    writeTo :: MonadIO m => ClientConnection -> ClientMessage -> m ()
    writeTo conn toSend = atomically $ writeTChan (writeChannel conn) (toSend, connectionId conn)

    sendTo :: MonadIO m => ClientConnection -> ServerMessage -> m ()
    sendTo conn toSend = atomically $ writeTChan (readChannel conn) toSend

    readFrom :: MonadIO m => ClientConnection -> m ServerMessage
    readFrom conn = atomically $ readTChan (readChannel conn)

instance NetworkConnections Connections where
    type WriteType Connections = ServerMessage
    type ReadType Connections = ClientMessage
    broadcast :: MonadIO m
        => Connections
        -> ServerMessage
        -> m ()
    broadcast Connections {..} state =
        forM_ players $ \conn ->
            atomically $ writeTChan (readChannel conn) state

addConnection :: Connections -> ClientConnection -> Connections
addConnection Connections {..} conn@ClientConnection {..} =
    Connections {players = insertMap connectionId conn players, ..}

removeConnection :: Connections -> ClientConnection -> Connections
removeConnection Connections {..} ClientConnection {..} =
    Connections {players = deleteMap connectionId players, ..}

newClientConnection
    :: Text
    -> WS.Connection
    -> TChan ServerMessage
    -> TChan (ClientMessage, Text)
    -> ClientConnection
newClientConnection = ClientConnection

addPlayer :: GameState -> InGamePlayer -> GameState
addPlayer GameState {..} igp = GameState
    { inGamePlayers = InGamePlayers (igp `cons` getInGamePlayers inGamePlayers)
    , ..
    }

removePlayer :: GameState -> InGamePlayer -> GameState
removePlayer GameState {..} igp = GameState
    { inGamePlayers = InGamePlayers
        (filter (/= igp) $ getInGamePlayers inGamePlayers)
    , ..
    }

modifyGameState :: (GameState -> a -> GameState) -> ServerState -> a -> ServerState
modifyGameState f ServerState{..} a = ServerState { gameState = f gameState a, ..}

modifyConnections :: (Connections -> a -> Connections) -> ServerState -> a -> ServerState
modifyConnections f ServerState{..} a = ServerState { connections = f connections a, ..}