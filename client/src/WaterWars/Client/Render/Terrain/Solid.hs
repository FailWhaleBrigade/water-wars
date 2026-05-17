module WaterWars.Client.Render.Terrain.Solid where

import GHC.Generics
import Miso (FromJSVal)
import WaterWars.Client.Render.Utils (LogicalLocation)


data Solid a =
    Solid
        { solidWidth :: Double
        , solidHeight :: Double
        , solidCenter :: LogicalLocation
        , solidContent :: a
        } deriving (Generic, Eq)

instance FromJSVal a => FromJSVal (Solid a) where
