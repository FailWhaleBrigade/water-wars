module Render.Display where

import ClassyPrelude
import Graphics.Gloss
import Data.Array.IArray hiding (index)

import WaterWars.Core.DefaultGame (defaultTerrain)
import WaterWars.Core.GameState (Terrain(..), BlockLocation(..))
import qualified WaterWars.Core.GameState as GameState

import Render.State
import Render.Config
import Render.Solid
import Render.Resources.Block

-- convert a game state into a picture
render :: MyGame -> IO Picture
render game = return $ pictures
    ([backgroundTexture game, player] ++ toList solidPictures ++ positions)
  where
    player =
        uncurry translate (playerLoc game) $ color playerColor $ circleSolid 20
    playerColor   = red
    solidPictures = map solidToPicture (solids game)
    terrainArray  = terrainBlocks defaultTerrain
    (BlockLocation (lowerX, upperX), BlockLocation (lowerY, upperY)) =
        bounds terrainArray
    mapWidth      = fromIntegral (upperX - lowerX) * blockSize
    mapHeight     = fromIntegral (upperY - lowerY) * blockSize
    mapWidthHalf  = mapWidth / 2
    mapHeightHalf = mapHeight / 2

    positions :: [Picture]
    positions = mapMaybe
        (\(loc, block) ->
            if block == GameState.SolidBlock
            then
                ( solidToPicture
                    . blockLocationToSolid mapWidthHalf
                                           mapHeightHalf
                                           blockSize
                                           loc
                    )
                    <$> (lookup Middle (blockMap game))
            else
                Nothing
        )
        (assocs terrainArray)

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid) (solidTexture solid)

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

