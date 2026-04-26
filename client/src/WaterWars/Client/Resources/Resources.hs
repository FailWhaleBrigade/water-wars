{-# LANGUAGE NoOverloadedStrings #-}

module WaterWars.Client.Resources.Resources where

import Control.Monad.IO.Class
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import GHC.Generics
import Miso.Prelude hiding ((.))
import WaterWars.Client.Codec.Resource (bulkLoad, loadPng)
import WaterWars.Client.Resources.Block (BlockMap, loadBlockMap)
import WaterWars.Client.Resources.Image (GameImage)
import WaterWars.Core.Terrain.Decoration
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Resources
  = Resources
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
  deriving (Generic, Eq)

newtype DecorationMap = DecorationMap {getDecorationMap :: Vector GameImage}
  deriving (Generic, Eq)

lookupDecorationMap :: Decoration -> DecorationMap -> GameImage
lookupDecorationMap val dm = getDecorationMap dm Vector.! fromEnum val

instance FromJSVal Resources
instance ToJSVal Resources
instance FromJSVal DecorationMap where
  fromJSVal val = fmap (DecorationMap . Vector.fromList) <$> fromJSVal val

instance ToJSVal DecorationMap where
  toJSVal val = toJSVal $ Vector.toList $ getDecorationMap val

setup :: (MonadIO m) => MisoString -> m Resources
setup baseUrl = do
  bgTex <- loadPng (baseUrl <> toMisoString "textures/background/background.png")
  prjTex <- loadPng (baseUrl <> toMisoString "textures/decoration/bubble.png")
  playerTex <- loadPng (baseUrl <> toMisoString "textures/mermaid/idle/mermaid1.png")
  playerRunningTexs <-
    bulkLoad
      (getMermaidPaths (baseUrl <> toMisoString "textures/mermaid/running/mermaid") 1 15)
  playerDeathTexs <-
    bulkLoad
      (getMermaidPaths (baseUrl <> toMisoString "textures/mermaid/death/mermaid_death") 1 9)
  mantaTexs <-
    bulkLoad
      [ baseUrl <> toMisoString "textures/manta_animation/manta1.png"
      , baseUrl <> toMisoString "textures/manta_animation/manta2.png"
      , baseUrl <> toMisoString "textures/manta_animation/manta3.png"
      , baseUrl <> toMisoString "textures/manta_animation/manta4.png"
      ]
  countdownTexs <-
    bulkLoad
      [ baseUrl <> toMisoString "textures/writing/3.png"
      , baseUrl <> toMisoString "textures/writing/2.png"
      , baseUrl <> toMisoString "textures/writing/1.png"
      , baseUrl <> toMisoString "textures/writing/GO.png"
      ]

  connectingTex <-
    bulkLoad
      [ baseUrl <> toMisoString "textures/writing/connecting0.png"
      , baseUrl <> toMisoString "textures/writing/connecting1.png"
      , baseUrl <> toMisoString "textures/writing/connecting2.png"
      , baseUrl <> toMisoString "textures/writing/connecting3.png"
      ]
  decorationTexsList <-
    bulkLoad
      [ baseUrl <> toMisoString "textures/decoration/algea.png"
      , baseUrl <> toMisoString "textures/decoration/coral.png"
      , baseUrl <> toMisoString "textures/decoration/snail.png"
      , baseUrl <> toMisoString "textures/decoration/umbrella.png"
      ]
  -- let
  --   decorationTypeList = [Algea, Coral, Snail, Umbrella]
  let
    decorationM =
      Vector.fromList decorationTexsList

  winTex <- loadPng $ baseUrl <> toMisoString "textures/writing/win.png"
  lostTex <- loadPng $ baseUrl <> toMisoString "textures/writing/lost.png"

  blockMap <- loadBlockMap baseUrl
  return $
    Resources
      { backgroundTexture = bgTex
      , projectileTexture = prjTex
      , idlePlayerTexture = playerTex
      , runningPlayerTextures = Foldable.toList playerRunningTexs
      , playerDeathTextures = Foldable.toList playerDeathTexs
      , mantaTextures = Foldable.toList mantaTexs
      , countdownTextures = Foldable.toList countdownTexs
      , blockMap = blockMap
      , connectingTextures = Foldable.toList connectingTex
      , youWinTexture = winTex
      , youLostTexture = lostTex
      , decorationMap = (DecorationMap decorationM)
      }

getMermaidPaths :: MisoString -> Int -> Int -> [MisoString]
getMermaidPaths pathStart ind mx
  | ind == mx =
      []
  | otherwise =
      (pathStart <> toMisoString (Text.show ind) <> toMisoString ".png") : getMermaidPaths pathStart (ind + 1) mx
