module WaterWars.Client.Network.Connection where

import ClassyPrelude

import WaterWars.Client.Render.State

connectionThread :: MonadIO m => NetworkConfig -> WorldSTM -> m ()
connectionThread _ _ = return ()

data NetworkConfig = NetworkConfig