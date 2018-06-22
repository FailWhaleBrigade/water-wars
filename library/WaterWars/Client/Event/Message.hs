module WaterWars.Client.Event.Message where

import ClassyPrelude
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified WaterWars.Network.Protocol as Protocol

data EventMessage
    = ServerEventMessage Protocol.ServerMessage
    | NetworkMetaMessage RequestedLogin
    | RenderEventMessage Gloss.Event
    | RenderTickEventMessage Float
    | SentEventMessage 
    deriving (Show, Eq)

data RequestedLogin = RequestedLogin deriving (Show, Eq, Read)
