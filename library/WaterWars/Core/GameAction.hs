module WaterWars.Core.GameAction where

import ClassyPrelude
import WaterWars.Core.GameState

data Action = Action
    { runAction :: Maybe RunAction
    , jumpAction :: Maybe JumpAction
    , shootAction :: Maybe Angle
    }
    deriving (Show, Read, Eq)

newtype RunAction = RunAction RunDirection
    deriving (Show, Read, Eq)

data RunDirection = RunLeft | RunRight
    deriving (Show, Read, Eq, Enum, Bounded)

data JumpAction = JumpAction
    deriving (Show, Read, Eq)

newtype ShootAction = ShootAction Angle
    deriving (Show, Read, Eq)

noAction :: Action
noAction = Action Nothing Nothing Nothing

runVelocityVector :: RunDirection -> VelocityVector
runVelocityVector RunLeft = VelocityVector (-0.5) 0
runVelocityVector RunRight = VelocityVector 0.5 0
