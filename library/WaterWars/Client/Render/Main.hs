module WaterWars.Client.Render.Main(main) where

import ClassyPrelude
import Control.Monad.Except

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Sound.ProteaAudio

import Options.Applicative

import WaterWars.Client.OptParse

import WaterWars.Client.Render.Update (handleKeysIO, updateIO)
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Display (renderIO)

import WaterWars.Client.Network.Connection (NetworkConfig(..), connectionThread)

window :: Display
window = InWindow "Water Wars" (800, 600) (10, 10)
-- window = Fullscreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

opts :: ParserInfo Arguments
opts = info
    (argumentsParser <**> helper)
    (  fullDesc
    <> progDesc "Start an instance of the water-wars client."
    <> header "Fail Whale Brigade presents Water Wars."
    )

main :: IO ()
main = do
    Arguments {..} <- execParser opts
    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    resourcesEither      <- runExceptT setup
    case resourcesEither of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right resources -> do
            worldStm <- initializeState resources
            _        <-
                async {- Should never terminate -}
                    (connectionThread Nothing
                                      (NetworkConfig port (unpack hostname))
                                      worldStm
                    )
            sample <- sampleFromFile "resources/sounds/Bubble_Game.ogg" 1.0
            soundLoop sample 1 1 0 1
            playIO window
                   backgroundColor
                   fps
                   worldStm
                   renderIO
                   handleKeysIO
                   updateIO
            -- Will never be reached
            putStrLn "Goodbye, shutting down the Server!"

