{-# LANGUAGE TemplateHaskell #-}

module WaterWars.Client.Network.Connection (module WaterWars.Client.Network.State, connectionThread) where

import ClassyPrelude

import qualified Network.WebSockets as WS

import Control.Monad.Logger

import Control.Concurrent

import WaterWars.Client.Render.State
import WaterWars.Client.Network.State (NetworkConfig(..), NetworkInfo(..), Connection, newConnection)

import WaterWars.Network.Protocol as Protocol
import WaterWars.Network.Connection

import WaterWars.Core.Game as CoreState

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
receiveUpdates (WorldSTM tvar) conn = runStdoutLoggingT $ forever $ do
    $logInfo "Wait for Game Update"
    serverMsg <- receive conn
    case serverMsg of
        Left  msg_ -> $logWarn $ "Could not read message: " ++ tshow msg_

        Right msg  -> atomically $ do
            world <- readTVar tvar
            let world' = updateWorld msg world
            writeTVar tvar world'

    return ()

sendUpdates :: MonadIO m => WorldSTM -> Connection -> m ()
sendUpdates (WorldSTM tvar) conn = runStdoutLoggingT $ forever $ do
    $logDebug $ "Send an update to the Server"
    world <- readTVarIO tvar
    let action = extractGameAction world
    $logDebug $ "Message: " ++ tshow action
    send conn (PlayerActionMessage action)
    liftIO $ threadDelay (1000000 `div` 60)
    return ()


updateWorld :: Protocol.ServerMessage -> World -> World
updateWorld serverMsg world@World {..} = case serverMsg of
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

            newOtherPlayers =
                maybe inGamePlayers_ (flip filter inGamePlayers_ . (/=)) player

            newProjectiles = getProjectiles $ gameProjectiles gameState

            worldInfo_     = WorldInfo
                { player       = join newPlayer -- TODO: can we express this better?
                , otherPlayers = newOtherPlayers
                , projectiles  = newProjectiles
                , ..
                }
        in
            World {worldInfo = worldInfo_, ..}

    GameSetupResponseMessage _ -> world

    LoginResponseMessage loginResponse ->
        let WorldInfo {..} = worldInfo
            newPlayer      = Just (successPlayer loginResponse)
            worldInfo_     = WorldInfo {player = newPlayer, ..}
        in  World {worldInfo = worldInfo_, ..}

extractGameAction :: World -> Protocol.PlayerAction
extractGameAction world =
    let
        WorldInfo {..} = worldInfo world
        runCmd | walkLeft  = Just (RunAction RunLeft)
               | walkRight = Just (RunAction RunRight)
               | otherwise = Nothing
        jmpCmd   = if jump then Just JumpAction else Nothing
        shootCmd = if shoot
            then Just $ angleForRunDirection lastDirection
            else Nothing
        playerAction = Action
            { runAction   = runCmd
            , jumpAction  = jmpCmd
            , shootAction = shootCmd
            }
    in
        PlayerAction {getAction = playerAction}
