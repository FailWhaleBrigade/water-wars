module Render.Main(main) where

import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Codec.Resource (loadPngAsBmp)
import Render.Update (handleKeysIO, updateIO)
import Render.State (initialState, Solid(..))
import Render.Resources.Tiles (Tile(..), loadTileMap, setTiles)
import Render.Display (render)
import Data.Maybe (fromJust)

window :: Display
window = InWindow "Water Wars" (800, 600) (10, 10)
-- window = Fullscreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

main :: IO ()
main = do
    bgTexEither <- loadPngAsBmp
        "resources/textures/background/background.png"
        1920
        1080
    Right tiles <- loadTileMap
    case bgTexEither of
        Left  err   -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right bgTex -> playIO window
                              backgroundColor
                              fps
                              (initialState bgTex (setTiles tiles))
                              render
                              handleKeysIO
                              updateIO

