{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Server.Client where

import ClassyPrelude

import Control.Monad.Logger

import WaterWars.Network.Connection
import WaterWars.Network.Protocol

clientGameThread
    :: ( MonadUnliftIO m
       , MonadIO m
       , NetworkConnection c
       , ReceiveType c ~ ClientMessage
       , SendType c ~ ServerMessage
       , MonadLogger m
       )
    => c --  ^Connection of the client
    -> (ClientMessage -> m ())
    -> m ServerMessage
    -> m ()
clientGameThread conn sendAction receiveAction = do
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    _ <- async (clientReceive conn sendAction)
    clientSend conn receiveAction


clientReceive
    :: (MonadIO m, MonadLogger m, NetworkConnection c, ReceiveType c ~ ClientMessage)
    => c --  ^Connection of the client
    -> (ClientMessage -> m ()) -- ^Send Message to Eventloop
    -> m ()
clientReceive conn sendAction = forever $ do
    $logDebug "Wait for data message" 
    msg <- receive conn
    case msg of
        Left msg_ -> do
            $logWarn "Could not read message"
            $logDebug $ "Could not read message: " ++ msg_
        Right playerAction -> do
            $logDebug $ "Read a message: " ++ tshow playerAction
            sendAction playerAction
            return ()
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes

clientSend
    :: (MonadIO m, MonadLogger m, NetworkConnection c, SendType c ~ ServerMessage)
    => c --  ^Connection of the client
    -> m ServerMessage
    -> m ()
clientSend conn receiveAction = forever $ do
    $logDebug "Wait for message"
    cmd <- receiveAction
    send conn cmd
    return ()
