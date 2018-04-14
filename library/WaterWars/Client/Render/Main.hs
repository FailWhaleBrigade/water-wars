module WaterWars.Client.Render.Main(main) where

import ClassyPrelude
import Control.Monad.Except

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import WaterWars.Client.Codec.Resource (loadPngAsBmp)
import WaterWars.Client.Resources.Block (loadBlockMap, BlockMap)

import WaterWars.Client.Render.Update (handleKeysIO, updateIO)
import WaterWars.Client.Render.State (initialState)
import WaterWars.Client.Render.Display (render)

window :: Display
window = InWindow "Water Wars" (800, 600) (10, 10)
-- window = Fullscreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

setup :: (MonadIO m, MonadError String m) => m (Picture, BlockMap)
setup = do
    bgTex    <- loadPngAsBmp "resources/textures/background/background.png"
    blockMap <- loadBlockMap
    return (bgTex, blockMap)

main :: IO ()
main = do
    resources <- runExceptT setup
    case resources of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right (bgTex, blocks) -> playIO window
                                        backgroundColor
                                        fps
                                        (initialState bgTex blocks)
                                        render
                                        handleKeysIO
                                        updateIO