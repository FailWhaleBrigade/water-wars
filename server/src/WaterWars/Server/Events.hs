module WaterWars.Server.Events where

import           ClassyPrelude
import           WaterWars.Core.Game

import           WaterWars.Network.Protocol
import           WaterWars.Server.ConnectionMgnt

type Connection = ClientConnection ServerMessage EventMessage

data EventMessage
    = EventClientMessage Text ClientMessage
    | EventGameLoopMessage GameState GameEvents
    | Register Text Connection

