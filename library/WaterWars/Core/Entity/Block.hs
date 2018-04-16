module WaterWars.Core.Entity.Block where

import ClassyPrelude

data Block
    = Floor
    | EndLeft
    | EndRight
    | BottomLeftCorner
    | BottomRightCorner
    | TopRightCorner
    | TopLeftCorner
    | LeftWall
    | RightWall
    | Middle
    | Ceil
    | NoBlock
    deriving (Show, Enum, Bounded, Eq, Ord, Read)