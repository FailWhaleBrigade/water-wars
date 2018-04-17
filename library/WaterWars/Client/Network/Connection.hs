module WaterWars.Client.Network.Connection (module WaterWars.Client.Network.State, connectionThread) where

import ClassyPrelude
import Network.WebSockets
import System.Log.Logger

import Control.Concurrent

import WaterWars.Client.Render.State
import WaterWars.Client.Network.State (NetworkConfig(..), NetworkInfo(..))
import qualified WaterWars.Network.Protocol as Protocol
import qualified WaterWars.Core.GameState as CoreState

connectionThread
    :: MonadIO m => Maybe NetworkInfo -> NetworkConfig -> WorldSTM -> m ()
connectionThread _ NetworkConfig {..} world = 
    liftIO $ runClient hostName portId "" (receiveUpdates world)


receiveUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
receiveUpdates (WorldSTM tvar) conn = forever $ do
    liftIO $ warningM "Server Connection" "Wait for Game Update"
    bs :: Text <- liftIO $ receiveData conn
    let maybeGameInfo = readMay bs :: Maybe Protocol.GameInformation
    case maybeGameInfo of
        Nothing ->
            liftIO
                .  warningM "Server Connection"
                $  "Could not parse the gameInfo: "
                ++ show bs

        Just info -> do
            liftIO $ warningM "Server Connection" "Received a game update"
            world <- readTVarIO tvar
            let world' = updateWorld info world
            atomically $ writeTVar tvar world'
    return ()

updateWorld :: Protocol.GameInformation -> World -> World
updateWorld (Protocol.Map gameMap) world@World {..} =
    setTerrain (blockMap renderInfo) (CoreState.gameTerrain gameMap) world

updateWorld (Protocol.State _) world@World {..} = world

-- TODO: send updates should issued by update loop
sendUpdates :: MonadIO m => WorldSTM -> Handle -> m ()
sendUpdates (WorldSTM tvar) h = forever $ do
    -- TODO: move this to bottom
    liftIO $ threadDelay (seconds 5.0)
    liftIO $ warningM "Server Connection" "Send an update to the Server"
    world <- readTVarIO tvar
    let action = extractGameAction world
    hPut h . encodeUtf8 $ tshow action
    return ()

extractGameAction :: World -> Protocol.PlayerAction
extractGameAction _ = undefined -- TODO: convert world information to action

seconds :: Float -> Int 
seconds = floor . (*1000000)