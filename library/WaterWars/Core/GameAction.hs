module WaterWars.Core.GameAction where

import ClassyPrelude
import WaterWars.Core.GameState

newtype Action = Action
    { rawActions :: Seq RawAction
    } deriving (Show, Read, Eq)

data RawAction
    = Run { runDirection :: RunDirection }
    | Jump
    | Shoot { shootAngle :: Angle }
    deriving (Show, Read, Eq)

data RunDirection = Left | Right deriving (Show, Read, Eq, Enum, Bounded)
