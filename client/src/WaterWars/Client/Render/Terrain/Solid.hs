module WaterWars.Client.Render.Terrain.Solid where

import WaterWars.Client.Resources.Image (GameImage)
import GHC.Generics
import Miso (FromJSVal)

data Solid =
    Solid
        { solidWidth :: Float
        , solidHeight :: Float
        , solidCenter :: (Float, Float)
        , solidTexture :: GameImage
        } deriving Generic

instance FromJSVal Solid where