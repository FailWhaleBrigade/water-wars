{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module WaterWars.Server.ConnectionMgnt
    ( ServerState(..)
    , Connections(..)
    , PlayerActions(..)
    , ClientConnection(connectionId, clientPlayer)
    , newClientConnection
    , addConnection
    , removeConnection
    ) where

import ClassyPrelude

import qualified Network.WebSockets as WS

import WaterWars.Network.Protocol
import WaterWars.Network.Connection

import WaterWars.Core.GameState
import WaterWars.Core.GameMap
import WaterWars.Core.GameAction

data ServerState = ServerState
    { connections :: Connections ServerMessage ClientMessage
    , gameMap     :: GameMap
    , gameState   :: GameState
    } deriving (Show, Eq)

newtype PlayerActions = PlayerActions 
    { getPlayerActions :: Map Player Action 
    } deriving (Show, Eq)

data Connections read write = Connections
    { players       :: Map Text (ClientConnection read write)
    , gameSetup     :: Maybe GameSetup
    } deriving (Show, Eq, Ord)

data ClientConnection read write = ClientConnection
    { connectionId  :: Text -- ^Session id, uniquely identifies players
    , clientPlayer  :: Player -- ^Player Meta information
    , connection    :: WS.Connection -- ^Abstraction over an connection handle
    , readChannel   :: TChan read -- ^Client threads read from this channel
    , writeChannel  :: TChan (write, Player) -- ^Client threads write to this channel, with its id
    }

instance Eq (ClientConnection read write) where
    c1 == c2 = connectionId c1 == connectionId c2

instance Ord (ClientConnection read write) where
    c1 <= c2 = connectionId c1 <= connectionId c2

instance Show (ClientConnection read write) where
    show ClientConnection {..} = "ClientConnection { connectionId = " ++ show connectionId ++ ", clientPlayer = " ++ show clientPlayer ++"}"

instance NetworkConnection (ClientConnection read write) where
    type SendType (ClientConnection read write) = read
    type ReceiveType (ClientConnection read write) = write
    send :: (MonadIO m, Serializable read) => ClientConnection read write -> read -> m ()
    send conn toSend = do
        let msg = serialize toSend
        liftIO $ WS.sendTextData (connection conn) msg

    receive :: (MonadIO m, Deserializable write) => ClientConnection read write -> m (Either Text write)
    receive conn = do
        msg <- liftIO $ WS.receiveData (connection conn)
        case deserialize msg of
            Nothing -> return $ Left msg
            Just action -> return $ Right action

instance IPC (ClientConnection read write) where
    type Identifier (ClientConnection read write) = Text 
    type WriteTo (ClientConnection read write) = write
    type ReadFrom (ClientConnection read write) = read

    writeTo :: MonadIO m => ClientConnection read write -> write -> m ()
    writeTo conn toSend = atomically $ writeTChan (writeChannel conn) (toSend, clientPlayer conn)

    readFrom :: MonadIO m => ClientConnection read write -> m read
    readFrom conn = atomically $ readTChan (readChannel conn)

instance NetworkConnections (Connections read write) where
    type WriteType (Connections read write) = read
    type ReadType (Connections read write) = write
    broadcast :: MonadIO m
        => Connections read write
        -> read
        -> m ()
    broadcast Connections {..} state =
        forM_ players $ \conn ->
            atomically $ writeTChan (readChannel conn) state

addConnection
    :: Connections read write
    -> ClientConnection read write
    -> Connections read write
addConnection Connections {..} conn@ClientConnection {..} =
    Connections {players = insertMap connectionId conn players, ..}

removeConnection
    :: Connections read write
    -> ClientConnection read write
    -> Connections read write
removeConnection Connections {..} ClientConnection {..} =
    Connections {players = deleteMap connectionId players, ..}

newClientConnection
    :: Text
    -> Player
    -> WS.Connection
    -> TChan read
    -> TChan (write, Player)
    -> ClientConnection read write
newClientConnection = ClientConnection
 