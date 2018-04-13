module WaterWars.Core.GameAction where

import ClassyPrelude
import WaterWars.Core.GameState

newtype Action = Action
    { rawActions :: Seq RawAction
    }

data RawAction
    = Run { runDirection :: RunDirection }
    | Jump
    | Shoot { shootAngle :: Angle }

data RunDirection = Left | Right
