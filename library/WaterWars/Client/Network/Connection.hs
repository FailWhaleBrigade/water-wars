{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Client.Network.Connection
    ( module WaterWars.Client.Network.State
    , module WaterWars.Client.Network.Connection
    )
where

import           ClassyPrelude

import qualified Network.WebSockets            as WS

import           Control.Monad.Logger

import           Control.Concurrent

import           WaterWars.Client.Event.Message

import           WaterWars.Client.Network.State

import           WaterWars.Network.Protocol    as Protocol
import           WaterWars.Network.Connection

import           WaterWars.Core.Game

connectionThread
    :: MonadIO m
    => Maybe NetworkInfo
    -> NetworkConfig
    -> TQueue EventMessage
    -> TQueue ClientMessage
    -> m ()
connectionThread _ NetworkConfig {..} broadcastChan receiveChan = forever $ do
    _ :: Either SomeException () <- liftIO $ try $ WS.runClient
        hostName
        portId
        ""
        (\conn -> do
            say "Connection has been opened"
            let connection = newConnection conn
            -- TODO: this setup code should be refactored soon-ish
            atomically $ writeTQueue broadcastChan
                                    (NetworkMetaMessage RequestedLogin)
            _ <- async $ receiveUpdates broadcastChan connection
            sendUpdates receiveChan connection
        )

    --case ret of
    --    Left (WS.CloseRequest _ _) ->
    --        say "Server requested to close the conenction"
    --    Left WS.ConnectionClosed     -> say "Connection has been closed"
    --    Left (WS.ParseException   _) -> say "Client sent impressive garbage"
    --    Left (WS.UnicodeException _) -> say "Weird unicode error"
    --    Right () ->
    --        say
    --            "Altoough weird, the connection was a success, whatever that means"
    say "Connection failed, retry in some time"
    liftIO $ threadDelay (1000000 * 5)



receiveUpdates :: MonadIO m => TQueue EventMessage -> Connection -> m ()
receiveUpdates broadcastTChan conn =
    runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ forever
        $ do
              $logDebug "Wait for Game Update"
              serverMsg <- receive conn
              case serverMsg of
                  Left msg_ ->
                      $logWarn $ "Could not read message: " ++ tshow msg_

                  Right msg -> atomically
                      $ writeTQueue broadcastTChan (ServerEventMessage msg)

              return ()

sendUpdates :: MonadIO m => TQueue ClientMessage -> Connection -> m ()
sendUpdates receiveChan conn =
    runStdoutLoggingT
        $ filterLogger (\_ level -> level /= LevelDebug)
        $ forever
        $ do
              $logDebug "Send an update to the Server"
              action <- atomically $ readTQueue receiveChan
              $logDebug $ "Message: " ++ tshow action
              send conn action
              liftIO $ threadDelay (1000000 `div` 50)
              return ()
