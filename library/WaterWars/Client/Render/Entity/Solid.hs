module WaterWars.Client.Render.Entity.Solid where

import ClassyPrelude
import Graphics.Gloss

data Solid =
    Solid
        { solidWidth :: Float
        , solidHeight :: Float
        , solidCenter :: (Float, Float)
        , solidTexture :: Picture
        } deriving (Show, Eq)