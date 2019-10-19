{-# LANGUAGE DataKinds #-}
module WaterWars.Server.ClientConnection where

import           ClassyPrelude
import           Control.Eff
import           Control.Eff.Lift        hiding ( lift )

import           WaterWars.Network.Protocol

import           WaterWars.Server.Events
import           WaterWars.Server.ConnectionMgnt

clientGameThread
    :: Connection -- ^Connection of the client
    -> (ClientMessage -> Eff '[Lift IO] ()) -- ^Send Message to Eventloop
    -> Eff '[Lift IO] ServerMessage -- ^Reads action to send from a monadic function
    -> IO () -- ^Should never return
clientGameThread conn sendAction receiveAction = race_
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    (clientReceive conn sendAction)
    (clientSend conn receiveAction)


clientReceive
    :: Connection -- ^Connection of the client
    -> (ClientMessage -> Eff '[Lift IO] ()) -- ^Send Message to Eventloop
    -> IO () -- ^Void or absurd, should never return
clientReceive conn sendAction =
    runLift
        . forever
        $ do -- Eff '[Lift IO] ()
              msg <- receive conn
              case msg of
                  Left msg_ -> return ()
                  Right playerAction -> do
                      sendAction playerAction
                      return ()
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes

clientSend
    :: Connection -- ^Connection of the client
    -> Eff '[Lift IO] ServerMessage -- ^Reads action to send from a monadic function
    -> IO () -- ^Void or absurd, should never return
clientSend conn receiveAction =
    runLift
        . forever
        $ do -- Eff '[Lift IO] ()
              cmd <- receiveAction
              send conn cmd
              return ()
