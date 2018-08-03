module WaterWars.Server.State 
    (SharedState(..)
    , FutureEvent (..)
    , EventMap
    , module WaterWars.Server.ConnectionMgnt
    ) where

import ClassyPrelude
import WaterWars.Core.Game

import WaterWars.Server.ConnectionMgnt

data SharedState =
    SharedState
        { eventQueue :: TQueue EventMessage
        , gameLoopTvar ::  TVar GameLoopState
        , playerActionTvar ::  TVar PlayerActions
        , connectionMapTvar ::  TVar (Map Text ClientConnection)
        , playerMapTvar  ::  TVar (Map Text InGamePlayer)
        , readyPlayersTvar ::  TVar (Set Text)
        , eventMapTvar :: TVar EventMap
        }

type EventMap = Map Integer FutureEvent

data FutureEvent 
    = ResetGame 
    | StartGame
    deriving (Show, Read, Eq, Ord, Enum)

