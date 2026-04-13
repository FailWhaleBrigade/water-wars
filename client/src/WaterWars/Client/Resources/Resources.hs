{-# LANGUAGE NoOverloadedStrings #-}
module WaterWars.Client.Resources.Resources where


import WaterWars.Core.Terrain.Decoration
import WaterWars.Client.Codec.Resource (loadPng, bulkLoad)
import WaterWars.Client.Resources.Block (loadBlockMap, BlockMap)
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import WaterWars.Client.Resources.Image (GameImage)
import qualified Data.Text as Text
import Miso.Prelude hiding ((.))
import GHC.Generics

data Resources =
    Resources
        { backgroundTexture :: GameImage
        , projectileTexture :: GameImage
        , idlePlayerTexture :: GameImage
        , runningPlayerTextures :: [GameImage]
        , playerDeathTextures :: [GameImage]
        , mantaTextures :: [GameImage]
        , countdownTextures :: [GameImage]
        , blockMap :: BlockMap
        , connectingTextures :: [GameImage]
        , youWinTexture :: GameImage
        , youLostTexture :: GameImage
        , decorationMap :: DecorationMap
        }
        deriving (Generic)
    -- deriving FromJSVal via (Generically Resources)

newtype DecorationMap = DecorationMap { getDecorationMap :: Map Decoration GameImage }
        deriving (Generic)

lookupDecorationMap :: Decoration -> DecorationMap -> Maybe GameImage
lookupDecorationMap val dm = Map.lookup val (getDecorationMap dm)

instance FromJSVal Resources where
instance ToJSVal Resources where
instance FromJSVal DecorationMap where
    fromJSVal val = fmap (DecorationMap . Map.mapKeys (read . fromMisoString)) <$> fromJSVal val

instance ToJSVal DecorationMap where
    toJSVal val = toJSVal $ Map.mapKeys (toMisoString . Text.show) $ getDecorationMap val

setup :: (MonadIO m) => MisoString -> m Resources
setup baseUrl = do
    bgTex <- loadPng (baseUrl <> toMisoString "textures/background/background.png")
    prjTex <- loadPng (baseUrl <> toMisoString "textures/decoration/bubble.png")
    playerTex <- loadPng (baseUrl <> toMisoString "textures/mermaid/idle/mermaid1.png")
    playerRunningTexs <- bulkLoad
        (getMermaidPaths (baseUrl <> toMisoString "textures/mermaid/running/mermaid") 1 15)
    playerDeathTexs <- bulkLoad
        (getMermaidPaths (baseUrl <> toMisoString "textures/mermaid/death/mermaid_death") 1 9)
    mantaTexs <- bulkLoad
        [ baseUrl <> toMisoString "textures/manta_animation/manta1.png"
        , baseUrl <> toMisoString "textures/manta_animation/manta2.png"
        , baseUrl <> toMisoString "textures/manta_animation/manta3.png"
        , baseUrl <> toMisoString "textures/manta_animation/manta4.png"
        ]
    countdownTexs <- bulkLoad
        [ baseUrl <> toMisoString "textures/writing/3.png"
        , baseUrl <> toMisoString "textures/writing/2.png"
        , baseUrl <> toMisoString "textures/writing/1.png"
        , baseUrl <> toMisoString "textures/writing/GO.png"
        ]

    connectingTex <- bulkLoad
        [ baseUrl <> toMisoString "textures/writing/connecting0.png"
        , baseUrl <> toMisoString "textures/writing/connecting1.png"
        , baseUrl <> toMisoString "textures/writing/connecting2.png"
        , baseUrl <> toMisoString "textures/writing/connecting3.png"
        ]
    decorationTexsList <- bulkLoad
        [ baseUrl <> toMisoString "textures/decoration/algea.png"
        , baseUrl <> toMisoString "textures/decoration/coral.png"
        , baseUrl <> toMisoString "textures/decoration/snail.png"
        , baseUrl <> toMisoString "textures/decoration/umbrella.png"
        ]
    let decorationTypeList = [Algea, Coral, Snail, Umbrella]
    let decorationM =
            Map.fromList $ zip decorationTypeList decorationTexsList

    winTex     <- loadPng $ baseUrl <> toMisoString "textures/writing/win.png"
    lostTex    <- loadPng $ baseUrl <> toMisoString "textures/writing/lost.png"

    blockMap   <- loadBlockMap baseUrl
    return $ Resources bgTex
                       prjTex
                    --    (scale 0.2 0.2 prjTex)
                       playerTex
                       (Foldable.toList playerRunningTexs)
                       (Foldable.toList playerDeathTexs)
                       (Foldable.toList mantaTexs)
                       (Foldable.toList countdownTexs)
                       blockMap
                       (Foldable.toList connectingTex)
                       winTex
                       lostTex
                       (DecorationMap decorationM)

getMermaidPaths :: MisoString -> Int -> Int -> [MisoString]
getMermaidPaths pathStart ind mx
    | ind == mx
    = []
    | otherwise
    = (pathStart <> toMisoString (Text.show ind) <> toMisoString ".png") : getMermaidPaths pathStart (ind + 1) mx

