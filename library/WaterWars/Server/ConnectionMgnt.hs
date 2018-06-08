{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module WaterWars.Server.ConnectionMgnt
    ( PlayerActions(..)
    , ClientConnection(..)
    , newClientConnection
    , addPlayer
    , removePlayer
    , module WaterWars.Server.EventQueue
    ) where

import ClassyPrelude

import qualified Network.WebSockets as WS

import WaterWars.Network.Protocol
import WaterWars.Network.Connection

import WaterWars.Core.Game

import WaterWars.Server.EventQueue

data ClientConnection = ClientConnection
    { connectionId  :: Text -- ^Session id, uniquely identifies players
    , connection    :: WS.Connection -- ^Abstraction over an connection handle
    , readChannel   :: TChan ServerMessage -- ^Client threads read from this channel
    , writeChannel  :: TChan EventMessage -- ^Client threads write to this channel
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

newClientConnection
    :: Text
    -> WS.Connection
    -> TChan ServerMessage
    -> TChan EventMessage
    -> ClientConnection
newClientConnection = ClientConnection
