module WaterWars.Core.Game.Action
    ( module WaterWars.Core.Game.Action
    , module WaterWars.Core.Game.Base
    )
where

import           ClassyPrelude
import           WaterWars.Core.Game.Base

data Action = Action
    { runAction :: Maybe RunAction
    , jumpAction :: Maybe JumpAction
    , shootAction :: Maybe Angle
    }
    deriving (Show, Read, Eq)

-- | right-biased semigroup. If a sub-action is present in both inputs, the
--   right one is kept in the resulting action.
instance Semigroup Action where
    Action r1 j1 s1 <> Action r2 j2 s2 =
        Action (r2 <|> r1) (j2 <|> j1) (s2 <|> s1)

instance Monoid Action where
    mempty = Action Nothing Nothing Nothing
    mappend = (<>)

newtype RunAction = RunAction RunDirection
    deriving (Show, Read, Eq)

data JumpAction = JumpAction
    deriving (Show, Read, Eq)

newtype ShootAction = ShootAction Angle
    deriving (Show, Read, Eq)

noAction :: Action
noAction = Action Nothing Nothing Nothing
