module WaterWars.Client.Network.Connection where

import ClassyPrelude
import Network
import WaterWars.Client.Render.State
import WaterWars.Core.GameState (gameTerrain) 

connectionThread :: MonadIO m => NetworkConfig -> WorldSTM -> m ()
connectionThread NetworkConfig {..} world =
    liftIO $ bracket (connectTo hostName portId) hClose (communicate world)

communicate :: MonadIO m => WorldSTM -> Handle -> m ()
communicate (WorldSTM tvar) h = do
    bs <- liftIO $ hGetContents h
    let Just gameMap = readMay $ decodeUtf8 bs
    atomically $ do 
        world <- readTVar tvar
        let world' = setTerrain (blockMap world) (gameTerrain gameMap) world
        writeTVar tvar world'
    return ()

data NetworkConfig = NetworkConfig
    { portId   :: PortID
    , hostName :: HostName
    } deriving (Show, Eq)

