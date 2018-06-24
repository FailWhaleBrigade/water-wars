module WaterWars.Client.Resources.Resources where

import ClassyPrelude
import Control.Monad.Except
import Sound.ProteaAudio
import Graphics.Gloss.Interface.IO.Game

import WaterWars.Client.Codec.Resource (loadPngAsBmp, bulkLoad)
import WaterWars.Client.Resources.Block (loadBlockMap, BlockMap)

data Resources =
    Resources
        { backgroundTexture :: Picture
        , projectileTexture :: Picture
        , idlePlayerTexture :: Picture
        , runningPlayerTextures :: [Picture]
        , playerDeathTextures :: [Picture]
        , mantaTextures :: [Picture]
        , countdownTextures :: [Picture]
        , blockMap :: BlockMap
        , connectingTextures :: [Picture]
        , youWinTexture :: Picture
        , youLostTexture :: Picture
        , shootSound :: Sample
        }

setup :: (MonadIO m, MonadError String m) => m Resources

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
    
    connectingTex <- bulkLoad $ fromList 
        [ "resources/textures/writing/connecting0.png"
        , "resources/textures/writing/connecting1.png"
        , "resources/textures/writing/connecting2.png"
        , "resources/textures/writing/connecting3.png"
        ]

    winTex <- loadPngAsBmp "resources/textures/writing/win.png"
    lostTex <- loadPngAsBmp "resources/textures/writing/lost.png"

    blockMap   <- loadBlockMap
    shootSound <- liftIO
        $ sampleFromFile "resources/sounds/bubble_into_glass.ogg" 1.0
    return $ Resources bgTex
                       (scale 0.2 0.2 prjTex)
                       playerTex
                       (toList playerRunningTexs)
                       (toList playerDeathTexs)
                       (toList mantaTexs)
                       (toList countdownTexs)
                       blockMap
                       (toList connectingTex)
                       winTex 
                       lostTex
                       shootSound

getMermaidPaths :: String -> Int -> Int -> [String]
getMermaidPaths pathStart ind mx
    | ind == mx
    = []
    | otherwise
    = (pathStart ++ show ind ++ ".png") : getMermaidPaths pathStart (ind + 1) mx
