{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module WaterWars.Client.Resources.Image where

import Data.Vector (Vector)
import GHC.Generics
import Miso.Prelude
import qualified Data.Vector as Vector

data GameImage = GameImage
  { image :: Image
  , imageSource :: MisoString
  }
  deriving (Generic)

instance Eq GameImage where
  a == b = imageSource a == imageSource b

instance Show GameImage where
  show a = show $ imageSource a

instance Ord GameImage where
  compare a b = compare (imageSource a) (imageSource b)

instance FromJSVal GameImage
instance ToJSVal GameImage

instance FromJSVal (Vector GameImage) where
  fromJSVal val = fmap Vector.fromList <$> fromJSVal val


instance ToJSVal (Vector GameImage)  where
  toJSVal val = toJSVal $ Vector.toList val

