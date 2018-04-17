module WaterWars.Network.Protocol where

import ClassyPrelude

import qualified WaterWars.Core.GameState as CoreState
import qualified WaterWars.Core.GameAction as CoreAction

data GameInformation
    = Map CoreState.GameMap
    | State CoreState.GameState
    deriving (Show, Read, Eq)

newtype PlayerAction =
    Action CoreAction.Action
    deriving (Show, Read, Eq)
