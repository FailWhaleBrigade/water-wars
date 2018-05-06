{-# LANGUAGE TypeFamilies #-}
module WaterWars.Server.Client where

import ClassyPrelude

import System.Log.Logger

import WaterWars.Network.Connection
import WaterWars.Server.Config

clientGameThread
    :: (MonadIO m, CanCommunicate c, Show (ReceiveType c))
    => c --  ^Connection of the client
    -> m ()
clientGameThread conn = do
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    _ <- liftIO $ async (clientReceive conn)
    clientSend conn


clientReceive
    :: (MonadIO m, CanCommunicate c, Show (ReceiveType c))
    => c --  ^Connection of the client
    -> m ()
clientReceive conn = forever $ do
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
            writeTo conn playerAction
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes
    return ()

clientSend
    :: (MonadIO m, CanCommunicate c)
    => c --  ^Connection of the client
    -> m ()
clientSend conn = forever $ do
    liftIO $ debugM "Server.Connection" "Wait for message"
    cmd <- readFrom conn
    send conn cmd
    return ()
