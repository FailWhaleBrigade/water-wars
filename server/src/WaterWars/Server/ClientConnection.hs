{-# LANGUAGE DataKinds #-}
module WaterWars.Server.ClientConnection where

import           ClassyPrelude
import           Effectful
import           Effectful.Log

import           WaterWars.Network.Protocol

import           WaterWars.Server.Events
import           WaterWars.Server.ConnectionMgnt

clientGameThread
    :: Logger -- ^Logger implemetation
    -> Connection -- ^Connection of the client
    -> (ClientMessage -> Eff '[Log, IOE] ()) -- ^Send Message to Eventloop
    -> Eff '[Log, IOE] ServerMessage -- ^Reads action to send from a monadic function
    -> IO () -- ^Should never return
clientGameThread logger conn sendAction receiveAction = race_
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    (clientReceive logger conn sendAction)
    (clientSend logger conn receiveAction)


clientReceive
    :: Logger -- ^Logger implemetation
    -> Connection -- ^Connection of the client
    -> (ClientMessage -> Eff '[Log, IOE] ()) -- ^Send Message to Eventloop
    -> IO () -- ^Void or absurd, should never return
clientReceive logger conn sendAction =
    runEff
        . runLog "clientReceive" logger LogInfo
        . forever
        $ do
              msg <- receive conn
              case msg of
                  Left msg_ -> do
                      logAttention_ ("Could not read message" :: Text)
                      logAttention_ ("Could not read message: " ++ tshow msg_)
                  Right playerAction -> do
                      --EffLog.logE ("Read a message: " ++ tshow playerAction)
                      sendAction playerAction
                      return ()
    -- TODO: should i sleep here for some time to avoid DOS-attack? yes

clientSend
    :: Logger -- ^Logger implemetation
    -> Connection -- ^Connection of the client
    -> Eff '[Log, IOE] ServerMessage -- ^Reads action to send from a monadic function
    -> IO () -- ^Void or absurd, should never return
clientSend logger conn receiveAction =
    runEff
    . runLog "clientSend" logger LogInfo
        . forever
        $ do -- Eff '[Log Text, Lift IO] ()
              -- EffLog.logE ("Wait for message" :: Text)
              cmd <- receiveAction
              send conn cmd
              return ()
