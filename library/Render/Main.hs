module Render.Main(main) where

import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Codec.Resource (loadPngAsBmp, bulkLoad, Width, Height)
import Render.Update (handleKeysIO, updateIO)
import Render.State (initialState, Solid(..))
import Render.Display (render)

window :: Display
window = FullScreen --InWindow "test" (200, 200) (10, 10)

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

main :: IO ()
main = do
    bgTexEither <- bulkLoad textures
    case bgTexEither of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right [bgTex, blockLeft, blockRight, block, centerBlock] -> 
            playIO 
                window 
                backgroundColor 
                fps 
                (initialState bgTex (singleton (Solid 50 50 (50, 50) blockRight))) 
                render
                handleKeysIO
                updateIO

textures :: [(FilePath, Width, Height)]
textures = 
    [ ("resources/textures/background/background.png", 1920, 1080)
    , ("resources/textures/block/blockendleft32.png", 32, 32)
    , ("resources/textures/block/blockendright32.png", 32, 32)
    , ("resources/textures/block/block32.png", 32, 32)
    , ("resources/textures/block/middleblock32.png", 32, 32)
    ]