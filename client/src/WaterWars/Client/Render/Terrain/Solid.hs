module WaterWars.Client.Render.Terrain.Solid where

import GHC.Generics
import Miso (FromJSVal)

data Solid a =
    Solid
        { solidWidth :: Float
        , solidHeight :: Float
        , solidCenter :: (Float, Float)
        , solidContent :: a
        } deriving (Generic, Eq)

instance FromJSVal a => FromJSVal (Solid a) where
