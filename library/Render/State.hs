module Render.State where

import ClassyPrelude
import Graphics.Gloss
import Data.Array.IArray

import WaterWars.Core.DefaultGame (defaultTerrain)
import WaterWars.Core.GameState (Terrain(..), BlockLocation(..))
import qualified WaterWars.Core.GameState as GameState

import Render.Solid
import Render.Config
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
initialState bmp blockMap' =
    Game 
        { playerLoc = (0, -50) --the bottom middle of the field
        , playerVel = (0, 0) -- not sure if we need velocity
        , backgroundTexture = bmp
        , solids = setBlocks blockMap' ++ fromList positions
        , blockMap = blockMap'
        }

    where
        terrainArray  = terrainBlocks defaultTerrain
        (BlockLocation (lowerX, upperX), BlockLocation (lowerY, upperY)) =
            bounds terrainArray
        mapWidth      = fromIntegral (upperX - lowerX) * blockSize
        mapHeight     = fromIntegral (upperY - lowerY) * blockSize
        mapWidthHalf  = mapWidth / 2
        mapHeightHalf = mapHeight / 2
    
        positions :: [Solid]
        positions = mapMaybe
            (\(loc, block) -> if block == GameState.SolidBlock
                then
                    blockLocationToSolid mapWidthHalf mapHeightHalf blockSize loc
    
                    <$> lookup Middle blockMap'
                else Nothing
            )
            (assocs terrainArray)
    

blockLocationToSolid
    :: Float -> Float -> Float -> BlockLocation -> Picture -> Solid
blockLocationToSolid mapWidthHalf mapHeightHalf size (BlockLocation (x, y)) picture
    = Solid
        { solidWidth   = size
        , solidHeight  = size
        , solidCenter  = ( (fromIntegral x) * size - mapWidthHalf
                         , (fromIntegral y) * size - mapHeightHalf
                         )
        , solidTexture = picture
        }
