{-# LANGUAGE NoOverloadedStrings #-}

module WaterWars.Client.Resources.Resources where

import Control.Monad.IO.Class
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics
import Miso.Prelude hiding ((.))
import WaterWars.Client.Codec.Resource (bulkLoad, loadPng)
import WaterWars.Client.Resources.Block (BlockMap, loadBlockMap)
import WaterWars.Client.Resources.Image (GameImage)
import WaterWars.Core.Terrain.Decoration

data Resources
  = Resources
  { backgroundTexture :: GameImage
  , projectileTexture :: GameImage
  , idlePlayerTexture :: GameImage
  , runningPlayerTextures :: Vector GameImage
  , playerDeathTextures :: Vector GameImage
  , mantaTextures :: Vector GameImage
  , countdownTextures :: Vector GameImage
  , blockMap :: BlockMap
  , connectingTextures :: Vector GameImage
  , youWinTexture :: GameImage
  , youLostTexture :: GameImage
  , decorationMap :: DecorationMap
  }
  deriving (Generic, Eq, Show)

instance FromJSVal Resources
instance ToJSVal Resources

newtype DecorationMap = DecorationMap {getDecorationMap :: Vector GameImage}
  deriving (Generic, Eq, Show)

instance FromJSVal DecorationMap where
  fromJSVal val = fmap (DecorationMap . Vector.fromList) <$> fromJSVal val

instance ToJSVal DecorationMap where
  toJSVal val = toJSVal $ Vector.toList $ getDecorationMap val

lookupDecorationMap :: Decoration -> DecorationMap -> GameImage
lookupDecorationMap val dm = getDecorationMap dm Vector.! fromEnum val

loadAssets :: (MonadIO m) => m Resources
loadAssets = do
  bgTex <- loadPng (toMisoString "textures/background/background.png")
  prjTex <- loadPng (toMisoString "textures/decoration/bubble.png")
  playerTex <- loadPng (toMisoString "textures/mermaid/idle/mermaid1.png")
  playerRunningTexs <-
    bulkLoad
      (getMermaidPaths (toMisoString "textures/mermaid/running/mermaid") 1 15)
  playerDeathTexs <-
    bulkLoad
      (getMermaidPaths (toMisoString "textures/mermaid/death/mermaid_death") 1 9)
  mantaTexs <-
    bulkLoad
      [ toMisoString "textures/manta_animation/manta1.png"
      , toMisoString "textures/manta_animation/manta2.png"
      , toMisoString "textures/manta_animation/manta3.png"
      , toMisoString "textures/manta_animation/manta4.png"
      ]
  countdownTexs <-
    bulkLoad
      [ toMisoString "textures/writing/3.png"
      , toMisoString "textures/writing/2.png"
      , toMisoString "textures/writing/1.png"
      , toMisoString "textures/writing/GO.png"
      ]

  connectingTex <-
    bulkLoad
      [ toMisoString "textures/writing/connecting0.png"
      , toMisoString "textures/writing/connecting1.png"
      , toMisoString "textures/writing/connecting2.png"
      , toMisoString "textures/writing/connecting3.png"
      ]
  decorationTexsList <-
    bulkLoad
      [ toMisoString "textures/decoration/algea.png"
      , toMisoString "textures/decoration/coral.png"
      , toMisoString "textures/decoration/snail.png"
      , toMisoString "textures/decoration/umbrella.png"
      ]
  -- let
  --   decorationTypeList = [Algea, Coral, Snail, Umbrella]
  let
    decorationM =
      Vector.fromList decorationTexsList

  winTex <- loadPng $ toMisoString "textures/writing/win.png"
  lostTex <- loadPng $ toMisoString "textures/writing/lost.png"

  blockMap <- loadBlockMap
  return $
    Resources
      { backgroundTexture = bgTex
      , projectileTexture = prjTex
      , idlePlayerTexture = playerTex
      , runningPlayerTextures = Vector.fromList playerRunningTexs
      , playerDeathTextures = Vector.fromList playerDeathTexs
      , mantaTextures = Vector.fromList mantaTexs
      , countdownTextures = Vector.fromList countdownTexs
      , blockMap = blockMap
      , connectingTextures = Vector.fromList connectingTex
      , youWinTexture = winTex
      , youLostTexture = lostTex
      , decorationMap = DecorationMap decorationM
      }

getMermaidPaths :: MisoString -> Int -> Int -> [MisoString]
getMermaidPaths pathStart ind mx
  | ind == mx =
      []
  | otherwise =
      (pathStart <> toMisoString (Text.show ind) <> toMisoString ".png") : getMermaidPaths pathStart (ind + 1) mx
