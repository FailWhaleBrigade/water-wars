module WaterWars.Client.Network.Connection (module WaterWars.Client.Network.State, connectionThread) where

import ClassyPrelude
import Network.WebSockets

import System.Log.Logger

import Control.Concurrent

import WaterWars.Client.Render.State
import WaterWars.Client.Network.State (NetworkConfig(..), NetworkInfo(..))
import qualified WaterWars.Network.Protocol as Protocol
import qualified WaterWars.Core.GameState as CoreState
import qualified WaterWars.Core.GameAction as CoreState

-- |Name of the component for the logger
networkLoggerName :: String
networkLoggerName = "Client.Connection"

connectionThread
    :: MonadIO m => Maybe NetworkInfo -> NetworkConfig -> WorldSTM -> m ()
connectionThread _ NetworkConfig {..} world = liftIO $ runClient
    hostName
    portId
    ""
    (\conn -> do
        _ <- async $ receiveUpdates world conn
        sendUpdates world conn
    )


receiveUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
receiveUpdates (WorldSTM tvar) conn = forever $ do
    liftIO $ warningM networkLoggerName "Wait for Game Update"
    bs :: Text <- liftIO $ receiveData conn
    let maybeGameInfo = readMay bs :: Maybe Protocol.GameInformation
    case maybeGameInfo of
        Nothing ->
            liftIO
                .  infoM networkLoggerName
                $  "Could not parse the gameInfo: "
                ++ show bs

        Just info -> do
            liftIO $ debugM networkLoggerName $ "Received a game update: " ++ show info
            world <- readTVarIO tvar
            let world' = updateWorld info world
            atomically $ writeTVar tvar world'
    return ()

updateWorld :: Protocol.GameInformation -> World -> World
updateWorld (Protocol.Map gameMap) world@World {..} =
    setTerrain (blockMap renderInfo) (CoreState.gameTerrain gameMap) world

updateWorld (Protocol.State gameState) World {..} = 
    let WorldInfo {..} = worldInfo
        newPlayer = fromMaybe player (headMay $ filter (== player) (CoreState.getInGamePlayers $ CoreState.inGamePlayers gameState))
        newOtherPlayers = filter (/= player) (CoreState.getInGamePlayers $ CoreState.inGamePlayers gameState)
        newProjectiles = CoreState.getProjectiles $ CoreState.gameProjectiles gameState
        worldInfo_ = WorldInfo { player = newPlayer, otherPlayers = newOtherPlayers, projectiles = newProjectiles ,..}
    in
        World { worldInfo = worldInfo_, ..}

sendUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
sendUpdates (WorldSTM tvar) conn = forever $ do
    -- TODO: move this to bottom
    liftIO $ threadDelay 1000000
    liftIO $ debugM networkLoggerName "Send an update to the Server"
    world <- readTVarIO tvar
    let action = extractGameAction world
    liftIO . sendTextData conn $ tshow action
    return ()

extractGameAction :: World -> Protocol.PlayerAction
extractGameAction world =
    let WorldInfo {..} = worldInfo world
        runCmd | walkLeft  = Just (CoreState.RunAction CoreState.RunLeft)
               | walkRight = Just (CoreState.RunAction CoreState.RunRight)
               | otherwise = Nothing
        jmpCmd       = if jump then Just CoreState.JumpAction else Nothing
        shootCmd     = if shoot then Just (CoreState.Angle 0) else Nothing
        playerAction = CoreState.Action
            { runAction   = runCmd
            , jumpAction  = jmpCmd
            , shootAction = shootCmd
            }
    in  Protocol.PlayerAction
            { action = playerAction
            , player = CoreState.playerDescription player
            }