module WaterWars.Client.Network.State where

import ClassyPrelude

data NetworkConfig = NetworkConfig
    { portId   :: Int
    , hostName :: String
    } deriving (Show, Eq)

data NetworkInfo = NetworkInfo
    { networkId     :: Text
    , networkConfig :: NetworkConfig
    } deriving (Eq, Show)
