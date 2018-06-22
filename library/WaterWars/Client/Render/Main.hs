module WaterWars.Client.Render.Main(main) where

import ClassyPrelude
import Control.Monad.Except

import Control.Monad.Logger

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

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
    => m (Picture, Picture, Picture, [Picture], [Picture], [Picture], [Picture], BlockMap)

setup = do
    bgTex <- loadPngAsBmp "resources/textures/background/background.png"
    prjTex <- loadPngAsBmp "resources/textures/decoration/bubble.png"
    playerTex <- loadPngAsBmp "resources/textures/mermaid/idle/mermaid1.png"
    playerRunningTexs <- bulkLoad $ fromList
        (getMermaidPaths "resources/textures/mermaid/running/mermaid" 1)
    playerDeathTexs <- bulkLoad $ fromList
        (getMermaidPaths "resources/textures/mermaid/death/mermaid_death" 1)
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
    blockMap <- loadBlockMap
    return (bgTex, prjTex, playerTex, toList playerRunningTexs, toList playerDeathTexs, toList mantaTexs, toList countdownTexs, blockMap)


getMermaidPaths :: String -> Int -> [String]
getMermaidPaths _ 15 = []
getMermaidPaths pathStart ind =
    (pathStart ++ show ind ++ ".png") : getMermaidPaths pathStart (ind + 1)


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
        Left  err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right (bgTex, prjTex, playerTex, playerRunningTexs, playerDeathTexs, mantaTexs, countdownTexs, blocks) -> do
            worldStm <- initializeState bgTex prjTex playerTex playerRunningTexs playerDeathTexs mantaTexs countdownTexs blocks
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
            -- Will never be reached
            putStrLn "Goodbye, shutting down the Server!"

