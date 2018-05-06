module WaterWars.Client.Network.Connection (module WaterWars.Client.Network.State, connectionThread) where

import ClassyPrelude

import qualified Network.WebSockets as WS

import System.Log.Logger

import Control.Concurrent

import WaterWars.Client.Render.State

import WaterWars.Client.Network.State (NetworkConfig(..), NetworkInfo(..), Connection, newConnection)

import WaterWars.Network.Protocol as Protocol
import WaterWars.Network.Connection

import WaterWars.Core.GameState ()
import WaterWars.Core.GameMap as CoreState
import WaterWars.Core.GameAction as CoreState


-- |Name of the component for the logger
networkLoggerName :: String
networkLoggerName = "Client.Connection"

connectionThread
    :: MonadIO m => Maybe NetworkInfo -> NetworkConfig -> WorldSTM -> m ()
connectionThread _ NetworkConfig {..} world = liftIO $ WS.runClient
    hostName
    portId
    ""
    (\conn -> do
        let connection = newConnection conn
        -- TODO: this setup code should be refactored soon-ish
        send connection (LoginMessage (Login Nothing))
        _ <- async $ receiveUpdates world connection
        sendUpdates world connection
    )


receiveUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
receiveUpdates (WorldSTM tvar) conn = forever $ do
    liftIO $ warningM networkLoggerName "Wait for Game Update"
    serverMsg <- receive conn
    case serverMsg of
        Left msg ->
            liftIO
                .  debugM networkLoggerName
                $  "Could not read message: "
                ++ show msg
        Right msg -> atomically $ do
            world <- readTVar tvar
            let world' = updateWorld msg world
            writeTVar tvar world'

    return ()

updateWorld :: Protocol.ServerMessage -> World -> World
updateWorld serverMsg world@World {..} =  
    case serverMsg of
        GameMapMessage gameMap -> 
            setTerrain (blockMap renderInfo) (gameTerrain gameMap) world

        GameStateMessage gameState ->
            let
                WorldInfo {..} = worldInfo
                inGamePlayers_ = getInGamePlayers $ inGamePlayers gameState
                newPlayer =
                    (\currentPlayer -> headMay $ filter
                            ((== playerDescription currentPlayer) . playerDescription)
                            inGamePlayers_
                        )
                        <$> player
        
                newOtherPlayers = maybe
                    inGamePlayers_
                    (flip filter inGamePlayers_ . (/=))
                    player
        
                newProjectiles = getProjectiles $ gameProjectiles gameState
        
                worldInfo_     = WorldInfo
                    { player       = join newPlayer -- TODO: can we express this better? 
                    , otherPlayers = newOtherPlayers
                    , projectiles  = newProjectiles
                    , ..
                    }
            in
                World {worldInfo = worldInfo_, ..}

        GameSetupResponseMessage _ -> 
            world

        LoginResponseMessage loginResponse -> 
            let
                WorldInfo {..} = worldInfo
                newPlayer = Just (successPlayer loginResponse)
                worldInfo_ = WorldInfo { player = newPlayer, ..}
            in
                World {worldInfo = worldInfo_, ..}

sendUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
sendUpdates (WorldSTM tvar) conn = forever $ do
    liftIO $ debugM networkLoggerName "Send an update to the Server"
    world <- readTVarIO tvar
    let action = extractGameAction world
    send conn (PlayerActionMessage action)
    liftIO $ threadDelay (1000000 `div` 60)
    return ()

extractGameAction :: World -> Protocol.PlayerAction
extractGameAction world =
    let
        WorldInfo {..} = worldInfo world
        runCmd | walkLeft  = Just (RunAction RunLeft)
               | walkRight = Just (RunAction RunRight)
               | otherwise = Nothing
        jmpCmd       = if jump then Just JumpAction else Nothing
        shootCmd     = if shoot then Just (Angle 0) else Nothing
        playerAction = Action
            { runAction   = runCmd
            , jumpAction  = jmpCmd
            , shootAction = shootCmd
            }
    in
        PlayerAction {getAction = playerAction}
