{-# LANGUAGE DerivingVia #-}
module WaterWars.Client.Resources.Image where

import Miso.Prelude
import GHC.Generics

data GameImage = GameImage
    { image :: Image
    , imageSource :: MisoString
    }
    deriving (Generic)

    -- deriving FromJSVal via (Generically GameImage)

instance FromJSVal GameImage where
instance ToJSVal GameImage where
