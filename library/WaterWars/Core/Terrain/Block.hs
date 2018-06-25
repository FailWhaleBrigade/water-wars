module WaterWars.Core.Terrain.Block where

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
    deriving (Show, Enum, Bounded, Eq, Ord, Read, Generic)

data Block
    = SolidBlock BlockContent
    | NoBlock
    deriving (Show, Eq, Ord, Read, Generic)

isSolid :: Block -> Bool
isSolid (SolidBlock _) = True
isSolid NoBlock        = False
