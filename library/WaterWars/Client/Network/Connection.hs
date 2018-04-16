module WaterWars.Client.Network.Connection (module WaterWars.Client.Network.State, connectionThread) where

import ClassyPrelude
import Network
import System.Log.Logger

import Control.Concurrent

import WaterWars.Client.Render.State (setTerrain, WorldSTM(..), World(..))
import WaterWars.Client.Network.State (NetworkConfig(..), NetworkInfo(..))
import qualified WaterWars.Core.GameState as CoreState
import qualified WaterWars.Core.GameAction as CoreAction

connectionThread
    :: MonadIO m => Maybe NetworkInfo -> NetworkConfig -> WorldSTM -> m ()
connectionThread _ config@NetworkConfig {..} world = liftIO $ bracket
    (do
        infoM "Server Connection" $ "Open Connection to: " ++ show config
        connectTo hostName portId
    )
    (\h -> do
        warningM "Server Connection" "Connection is closed now"
        hClose h
    )
    -- TODO: this function swallows things
    (\h -> race_ (recieveUpdates world h) (sendUpdates world h))


recieveUpdates :: MonadIO m => WorldSTM -> Handle -> m ()
recieveUpdates (WorldSTM tvar) h = forever $ do
    bs <- liftIO $ hGetContents h
    let maybeGameInfo = readMay $ decodeUtf8 bs
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

updateWorld :: CoreState.GameInformation -> World -> World
updateWorld (CoreState.Map gameMap) world@World {..} =
    setTerrain blockMap (CoreState.gameTerrain gameMap) world

updateWorld (CoreState.State _) world@World {..} = world

sendUpdates :: MonadIO m => WorldSTM -> Handle -> m ()
sendUpdates (WorldSTM tvar) h = forever $ do
    -- TODO: move this to bottom
    liftIO $ threadDelay (seconds 1.0)
    liftIO $ warningM "Server Connection" "Send an update to the Server"
    world <- readTVarIO tvar
    let action = extractGameAction world
    hPut h . encodeUtf8 $ tshow action
    return ()

extractGameAction :: World -> CoreAction.Action
extractGameAction _ = undefined -- TODO: convert world information to action

seconds :: Float -> Int 
seconds = floor . (*1000000)