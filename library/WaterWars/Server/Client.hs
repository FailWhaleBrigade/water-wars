{-# LANGUAGE TypeFamilies #-}
module WaterWars.Server.Client where

import ClassyPrelude

import System.Log.Logger

import WaterWars.Network.Connection
import WaterWars.Network.Protocol
import WaterWars.Server.Config

clientGameThread
    :: (MonadUnliftIO m, MonadIO m, NetworkConnection c, ReceiveType c ~ ClientMessage, SendType c ~ ServerMessage)
    => c --  ^Connection of the client
    -> (ClientMessage -> m ())
    -> m ServerMessage
    -> m ()
clientGameThread conn sendAction receiveAction = do
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    _ <- async (clientReceive conn sendAction)
    clientSend conn receiveAction


clientReceive
    :: (MonadIO m, NetworkConnection c, ReceiveType c ~ ClientMessage)
    => c --  ^Connection of the client
    -> (ClientMessage -> m ()) -- ^Send Message to Eventloop
    -> m ()
clientReceive conn sendAction = forever $ do
    liftIO $ debugM "Server.Connection" "Wait for data message"
    msg <- receive conn
    case msg of
        Left msg_ -> do
            liftIO $ infoM networkLoggerName "Could not read message"
            liftIO $ debugM networkLoggerName
                            ("Could not read message: " ++ show msg_)
        Right playerAction -> do
            liftIO
                $  infoM networkLoggerName
                $  "Read a message: "
                ++ show playerAction
            sendAction playerAction
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes
    return ()

clientSend
    :: (MonadIO m, NetworkConnection c, SendType c ~ ServerMessage)
    => c --  ^Connection of the client
    -> m ServerMessage
    -> m ()
clientSend conn receiveAction = forever $ do
    liftIO $ debugM "Server.Connection" "Wait for message"
    cmd <- receiveAction
    send conn cmd
    return ()