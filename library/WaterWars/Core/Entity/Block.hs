module WaterWars.Core.Entity.Block where

import ClassyPrelude

data BlockContent
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
    deriving (Show, Enum, Bounded, Eq, Ord, Read)

data Block 
    = SolidBlock BlockContent
    | NoBlock 
    deriving (Show, Eq, Ord, Read)