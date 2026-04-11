module WaterWars.Client.Resources.Resources where

import           Data.Text (Text)
import Control.Monad.Except
import Graphics.Gloss.Interface.IO.Game

import WaterWars.Core.Terrain.Decoration
import WaterWars.Client.Codec.Resource (loadPngAsBmp, bulkLoad)
import WaterWars.Client.Resources.Block (loadBlockMap, BlockMap)
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map

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
        , decorationMap :: Map Decoration Picture
        }

setup :: (MonadIO m, MonadError Text m) => m Resources
setup = do
    bgTex <- loadPngAsBmp "resources/textures/background/background.png"
    prjTex <- loadPngAsBmp "resources/textures/decoration/bubble.png"
    playerTex <- loadPngAsBmp "resources/textures/mermaid/idle/mermaid1.png"
    playerRunningTexs <- bulkLoad
        (getMermaidPaths "resources/textures/mermaid/running/mermaid" 1 15)
    playerDeathTexs <- bulkLoad
        (getMermaidPaths "resources/textures/mermaid/death/mermaid_death" 1 9)
    mantaTexs <- bulkLoad
        [ "resources/textures/manta_animation/manta1.png"
        , "resources/textures/manta_animation/manta2.png"
        , "resources/textures/manta_animation/manta3.png"
        , "resources/textures/manta_animation/manta4.png"
        ]
    countdownTexs <- bulkLoad
        [ "resources/textures/writing/3.png"
        , "resources/textures/writing/2.png"
        , "resources/textures/writing/1.png"
        , "resources/textures/writing/GO.png"
        ]

    connectingTex <- bulkLoad
        [ "resources/textures/writing/connecting0.png"
        , "resources/textures/writing/connecting1.png"
        , "resources/textures/writing/connecting2.png"
        , "resources/textures/writing/connecting3.png"
        ]
    decorationTexsList <- bulkLoad
        [ "resources/textures/decoration/algea.png"
        , "resources/textures/decoration/coral.png"
        , "resources/textures/decoration/snail.png"
        , "resources/textures/decoration/umbrella.png"
        ]
    let decorationTypeList = [Algea, Coral, Snail, Umbrella]
    let decorationM =
            Map.fromList $ zip decorationTypeList decorationTexsList

    winTex     <- loadPngAsBmp "resources/textures/writing/win.png"
    lostTex    <- loadPngAsBmp "resources/textures/writing/lost.png"

    blockMap   <- loadBlockMap
    return $ Resources bgTex
                       (scale 0.2 0.2 prjTex)
                       playerTex
                       (Foldable.toList playerRunningTexs)
                       (Foldable.toList playerDeathTexs)
                       (Foldable.toList mantaTexs)
                       (Foldable.toList countdownTexs)
                       blockMap
                       (Foldable.toList connectingTex)
                       winTex
                       lostTex
                       decorationM

getMermaidPaths :: String -> Int -> Int -> [String]
getMermaidPaths pathStart ind mx
    | ind == mx
    = []
    | otherwise
    = (pathStart ++ show ind ++ ".png") : getMermaidPaths pathStart (ind + 1) mx

