module WaterWars.Core.Terrain.Decoration where
import GHC.Generics

data Decoration
    = Algea
    | Coral
    | Snail
    | Umbrella
    deriving (Show, Enum, Bounded, Eq, Ord, Read, Generic)
