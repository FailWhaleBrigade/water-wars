module WaterWars.Client.Network.Connection where

import ClassyPrelude
import Network
import System.Log.Logger

import WaterWars.Client.Render.State
import qualified WaterWars.Core.GameState as CoreState

connectionThread :: MonadIO m => NetworkConfig -> WorldSTM -> m ()
connectionThread NetworkConfig {..} world =
    liftIO $ bracket (connectTo hostName portId) hClose (communicate world)

communicate :: MonadIO m => WorldSTM -> Handle -> m ()
communicate (WorldSTM tvar) h = forever $ do
    bs <- liftIO $ hGetContents h
    let maybeGameInfo = readMay $ decodeUtf8 bs
    case maybeGameInfo of
        Nothing ->
            liftIO
                .  warningM "Server Connection"
                $  "Could not parse the gameInfo: "
                ++ show bs
        Just info -> atomically $ do
            world <- readTVar tvar
            let world' = updateWorld info world
            writeTVar tvar world'
    return ()

updateWorld :: CoreState.GameInformation -> World -> World
updateWorld (CoreState.Map gameMap) world@World {..} =
    setTerrain blockMap (CoreState.gameTerrain gameMap) world
updateWorld (CoreState.State _) world@World {..} = world

data NetworkConfig = NetworkConfig
    { portId   :: PortID
    , hostName :: HostName
    } deriving (Show, Eq)

