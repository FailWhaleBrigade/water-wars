module WaterWars.Client.Network.State where

import ClassyPrelude

import Network

data NetworkConfig = NetworkConfig
    { portId   :: PortID
    , hostName :: HostName
    } deriving (Show, Eq)

data NetworkInfo = NetworkInfo
    { networkId     :: Text
    , networkConfig :: NetworkConfig
    } deriving (Eq, Show)
