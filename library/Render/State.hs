module Render.State where

import ClassyPrelude
import Graphics.Gloss
import Render.Solid
import Render.Resources.Block

type Radius = Float

type Position = (Float, Float)

data MyGame = Game 
    { playerLoc :: (Float, Float)
    , playerVel :: (Float, Float)
    , backgroundTexture :: Picture
    , blockMap :: BlockMap
    , solids :: Seq Solid 
    } deriving Show

initialState :: Picture -> BlockMap -> MyGame
initialState bmp blockMap =
    Game 
        { playerLoc = (0, -50) --the bottom middle of the field
        , playerVel = (0, 0) -- not sure if we need velocity
        , backgroundTexture = bmp
        , solids = setBlocks blockMap
        , blockMap = blockMap
        }
