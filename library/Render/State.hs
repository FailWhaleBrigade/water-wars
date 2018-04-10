module Render.State where

import ClassyPrelude
import Graphics.Gloss

type Radius = Float

type Position = (Float, Float)

fieldWidth :: Float
fieldWidth = 150

fieldHeight :: Float
fieldHeight = 150

data MyGame = Game 
    { playerLoc :: (Float, Float)
    , playerVel :: (Float, Float)
    , backgroundTexture :: Picture
    } deriving Show

initialState :: Picture -> MyGame
initialState bmp =
    Game 
        { playerLoc = (0, -50) --the bottom middle of the field
        , playerVel = (0, 0) -- not sure if we need velocity
        , backgroundTexture = bmp
        }
        