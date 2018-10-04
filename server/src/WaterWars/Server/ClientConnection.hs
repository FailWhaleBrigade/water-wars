{-# LANGUAGE DataKinds #-}
module WaterWars.Server.ClientConnection where

import           ClassyPrelude
import           Control.Eff
import           Control.Eff.Log
import qualified Control.Eff.Log               as EffLog
import           Control.Eff.Lift        hiding ( lift )

import           WaterWars.Network.Connection
import           WaterWars.Network.Protocol

import           WaterWars.Server.Events

clientGameThread
    :: Logger IO Text -- ^Logger implemetation
    -> Connection -- ^Connection of the client
    -> (ClientMessage -> Eff '[Log Text, Lift IO] ()) -- ^Send Message to Eventloop
    -> Eff '[Log Text, Lift IO] ServerMessage -- ^Reads action to send from a monadic function
    -> IO () -- ^Should never return
clientGameThread logger conn sendAction receiveAction = race_
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    (clientReceive logger conn sendAction)
    (clientSend logger conn receiveAction)


clientReceive
    :: Logger IO Text -- ^Logger implemetation
    -> Connection -- ^Connection of the client
    -> (ClientMessage -> Eff '[Log Text, Lift IO] ()) -- ^Send Message to Eventloop
    -> IO () -- ^Void or absurd, should never return
clientReceive logger conn sendAction =
    runLift
        . runLog logger
        . forever
        $ do -- Eff '[Log Text, Lift IO] ()
              -- EffLog.logE ("Wait for data message" :: Text)
              msg <- receive conn
              case msg of
                  Left msg_ -> do
                      EffLog.logE ("Could not read message" :: Text)
                      EffLog.logE ("Could not read message: " ++ tshow msg_)
                  Right playerAction -> do
                      --EffLog.logE ("Read a message: " ++ tshow playerAction)
                      sendAction playerAction
                      return ()
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes

clientSend
    :: Logger IO Text -- ^Logger implemetation
    -> Connection -- ^Connection of the client
    -> Eff '[Log Text, Lift IO] ServerMessage -- ^Reads action to send from a monadic function
    -> IO () -- ^Void or absurd, should never return
clientSend logger conn receiveAction =
    runLift
        . runLog logger
        . forever
        $ do -- Eff '[Log Text, Lift IO] ()
              -- EffLog.logE ("Wait for message" :: Text)
              cmd <- receiveAction
              send conn cmd
              return ()
