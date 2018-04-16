module WaterWars.Client.Network.Connection where

import ClassyPrelude

import Network
import WaterWars.Client.Render.State

connectionThread :: MonadIO m => NetworkConfig -> WorldSTM -> m ()
connectionThread NetworkConfig {..} world =
    liftIO $ bracket (connectTo hostName portId) (communicate world) (hClose)

communicate :: MonadIO m => WorldSTM -> Handle -> m ()
communicate _ _ = return ()

data NetworkConfig = NetworkConfig
    { portId   :: PortID
    , hostName :: HostName
    } deriving (Show, Eq)

