module WaterWars.Client.Render.Main(main) where

import ClassyPrelude
import Control.Monad.Except

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import WaterWars.Client.Codec.Resource (loadPngAsBmp, bulkLoad)
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

setup :: (MonadIO m, MonadError String m) => m (Picture, Picture, [Picture], BlockMap)
setup = do
    bgTex    <- loadPngAsBmp "resources/textures/background/background.png"
    prjTex <- loadPngAsBmp "resources/textures/decoration/bubble.png"
    mantaTexs <- bulkLoad $ fromList ["resources/textures/manta_animation/manta1.png", "resources/textures/manta_animation/manta2.png", "resources/textures/manta_animation/manta3.png",  "resources/textures/manta_animation/manta4.png"]
    blockMap <- loadBlockMap
    return (bgTex, prjTex, toList mantaTexs, blockMap)

main :: IO ()
main = do
    resources <- runExceptT setup
    case resources of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right (bgTex, prjTex, mantaTexs, blocks) -> do
            worldStm <- initializeState bgTex prjTex mantaTexs blocks
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
            putStrLn "Goodbye, shuting down the Server!"
