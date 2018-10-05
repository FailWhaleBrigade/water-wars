module WaterWars.Server.Events where

import           WaterWars.Core.Game

import           WaterWars.Network.Protocol
import           WaterWars.Server.ConnectionMgnt

type Connection = ClientConnection ServerMessage EventMessage

data EventMessage
    = ClientMessageEvent Player ClientMessage
    | GameLoopMessageEvent GameState GameEvents
    | RegisterEvent Player Connection

