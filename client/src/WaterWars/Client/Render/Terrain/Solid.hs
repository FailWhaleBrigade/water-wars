module WaterWars.Client.Render.Terrain.Solid where

import GHC.Generics
import Miso (FromJSVal)
import WaterWars.Client.Render.Utils (LogicalLocation (..))


data Solid a =
    Solid
        { solidWidth :: Double
        , solidHeight :: Double
        , solidCenter :: LogicalLocation
        , solidContent :: a
        } deriving (Generic, Eq)

solidTopLeft :: Solid a -> (Double, Double)
solidTopLeft s =
    let
        LogicalLocation (x, y) = solidCenter s
    in
        ( fromIntegral x - 1 / 2
        , fromIntegral y + 1 / 2
        )

instance FromJSVal a => FromJSVal (Solid a) where
