module WaterWars.Client.Render.Main(main) where

import ClassyPrelude
import Control.Monad.Except

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import WaterWars.Client.Codec.Resource (loadPngAsBmp)
import WaterWars.Client.Resources.Block (loadBlockMap, BlockMap)

import WaterWars.Client.Render.Update (handleKeysIO, updateIO)
import WaterWars.Client.Render.State (initializeState)
import WaterWars.Client.Render.Display (renderIO)

import WaterWars.Client.Network.Connection (NetworkConfig(..), connectionThread)

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
        Right (bgTex, blocks) -> do
            worldStm <- initializeState bgTex blocks
            _        <-
                async {- Should never terminate -}
                    (connectionThread Nothing
                                      (NetworkConfig 1234 "localhost")
                                      worldStm
                    )
            playIO window
                   backgroundColor
                   fps
                   worldStm
                   renderIO
                   handleKeysIO
                   updateIO
