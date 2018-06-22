module WaterWars.Client.Render.Main(main) where

import ClassyPrelude
import Control.Monad.Except

import Control.Monad.Logger

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Sound.ProteaAudio

import Options.Applicative

import WaterWars.Client.OptParse

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

setup
    :: (MonadIO m, MonadError String m)
    => m
           ( Picture
           , Picture
           , Picture
           , [Picture]
           , [Picture]
           , [Picture]
           , [Picture]
           , BlockMap
           , Sample
           )

setup = do
    bgTex <- loadPngAsBmp "resources/textures/background/background.png"
    prjTex <- loadPngAsBmp "resources/textures/decoration/bubble.png"
    playerTex <- loadPngAsBmp "resources/textures/mermaid/idle/mermaid1.png"
    playerRunningTexs <- bulkLoad $ fromList
        (getMermaidPaths "resources/textures/mermaid/running/mermaid" 1 15)
    playerDeathTexs <- bulkLoad $ fromList
        (getMermaidPaths "resources/textures/mermaid/death/mermaid_death" 1 9)
    mantaTexs <- bulkLoad $ fromList
        [ "resources/textures/manta_animation/manta1.png"
        , "resources/textures/manta_animation/manta2.png"
        , "resources/textures/manta_animation/manta3.png"
        , "resources/textures/manta_animation/manta4.png"
        ]
    countdownTexs <- bulkLoad $ fromList
        [ "resources/textures/writing/3.png"
        , "resources/textures/writing/2.png"
        , "resources/textures/writing/1.png"
        , "resources/textures/writing/GO.png"
        ]
    blockMap   <- loadBlockMap
    shootSound <- liftIO
        $ sampleFromFile "resources/sounds/bubble_into_glass.ogg" 1.0
    return
        ( bgTex
        , prjTex
        , playerTex
        , toList playerRunningTexs
        , toList playerDeathTexs
        , toList mantaTexs
        , toList countdownTexs
        , blockMap
        , shootSound
        )


getMermaidPaths :: String -> Int -> Int -> [String]
getMermaidPaths pathStart ind mx
    | ind == mx
    = []
    | otherwise
    = (pathStart ++ show ind ++ ".png") : getMermaidPaths pathStart (ind + 1) mx


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
    resources      <- runExceptT setup
    case resources of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right (bgTex, prjTex, playerTex, playerRunningTexs, playerDeathTexs, mantaTexs, countdownTexs, blocks, shootSound)
            -> do
                worldStm <- initializeState bgTex
                                            prjTex
                                            playerTex
                                            playerRunningTexs
                                            playerDeathTexs
                                            mantaTexs
                                            countdownTexs
                                            blocks
                                            shootSound
                _ <-
                    async {- Should never terminate -}
                        (connectionThread Nothing
                                          (NetworkConfig 1234 "localhost")
                                          worldStm
                        )
                result <- initAudio 64 44100 512 -- max channels, mixing frequency, mixing buffer size
                unless result $ fail "failed to initialize the audio system"
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
