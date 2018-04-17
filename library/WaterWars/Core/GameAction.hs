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

data RunDirection = RunLeft | RunRight deriving (Show, Read, Eq, Enum, Bounded)

isRunAction :: RawAction -> Bool
isRunAction (Run _) = True
isRunAction _ = False
