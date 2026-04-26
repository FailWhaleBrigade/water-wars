{-# LANGUAGE DerivingVia #-}
module WaterWars.Client.Resources.Image where

import Miso.Prelude
import GHC.Generics

data GameImage = GameImage
    { image :: Image
    , imageSource :: MisoString
    }
    deriving (Generic)

instance Eq GameImage where
    a == b = imageSource a == imageSource b

instance Ord GameImage where
    compare a b = compare (imageSource a) (imageSource b)

instance FromJSVal GameImage where
instance ToJSVal GameImage where
