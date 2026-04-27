module WaterWars.Client.Render.Terrain.Solid where

import GHC.Generics
import Miso (FromJSVal)

data Solid a =
    Solid
        { solidWidth :: Double
        , solidHeight :: Double
        , solidCenter :: (Double, Double)
        , solidContent :: a
        } deriving (Generic, Eq)

instance FromJSVal a => FromJSVal (Solid a) where
