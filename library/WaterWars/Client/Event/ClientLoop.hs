module WaterWars.Client.Event.ClientLoop where

import ClassyPrelude

import Control.Monad.Logger

import Graphics.Gloss.Interface.IO.Game as Gloss

import WaterWars.Client.Render.Animation
import WaterWars.Client.Render.Config
import WaterWars.Client.Render.State

import WaterWars.Client.Event.Message

import WaterWars.Network.Protocol as Protocol

import WaterWars.Core.Game

eventLoop
    :: (MonadIO m)
    => WorldSTM
    -> TVar PlayerAction
    -> TQueue EventMessage
    -> TQueue ClientMessage
    -> m ()
eventLoop worldStm@(WorldSTM worldTvar) actionTvar broadcast receiveChannel =
    forever $ do
        message <- atomically $ readTQueue broadcast
        case message of
            ServerEventMessage serverMessage -> atomically $ do
                world <- readTVar worldTvar
                let world' = handleServerMessage serverMessage world
                writeTVar worldTvar world'

            RenderEventMessage event ->
                handleKeys event worldStm actionTvar receiveChannel
            RenderTickEventMessage delta -> do
                updateIO delta worldStm
                playerAction <- readTVarIO actionTvar
                unless (isEmpty playerAction) $ atomically $ sendMessage
                    receiveChannel
                    (PlayerActionMessage playerAction)
            NetworkMetaMessage RequestedLogin ->
                sendMessageIO receiveChannel (LoginMessage $ Login Nothing)
            SentEventMessage -> do
                playerAction <- readTVarIO actionTvar
                atomically $ sendMessage receiveChannel
                                         (PlayerActionMessage playerAction)
        return ()


handleServerMessage :: Protocol.ServerMessage -> World -> World
handleServerMessage serverMsg world@World {..} = case serverMsg of
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

    GameStartMessage (GameStart n) -> world

handleKeys
    :: MonadIO m
    => Event
    -> WorldSTM
    -> TVar PlayerAction
    -> TQueue ClientMessage
    -> m ()
handleKeys (EventKey (Char c) Gloss.Down _ _) _ actionTvar _
    | c == 'a' = atomically $ modifyTVar actionTvar (setRunAction RunLeft)
    | c == 'w' = atomically $ modifyTVar actionTvar setJumpAction
    | c == 'd' = atomically $ modifyTVar actionTvar (setRunAction RunRight)
handleKeys (EventKey (Char c) Gloss.Up _ _) _ actionTvar _
    | c == 'a' = atomically $ modifyTVar actionTvar unsetRunAction
    | c == 'w' = atomically $ modifyTVar actionTvar unsetJumpAction
    | c == 'd' = atomically $ modifyTVar actionTvar unsetRunAction

handleKeys (EventKey (SpecialKey KeyEnter) Gloss.Up _ _) _ _ receiveChannel =
    sendMessageIO receiveChannel (ClientReadyMessage ClientReady)

handleKeys (EventKey (MouseButton LeftButton) Gloss.Up _ (x, y)) (WorldSTM tvar) _ receiveChannel
    = do
        World {..} <- readTVarIO tvar
        case player worldInfo of
            Nothing                -> return ()
            Just InGamePlayer {..} -> sendMessageIO
                receiveChannel
                (PlayerActionMessage $ createShootAction $ calculateAngle
                    playerLocation
                    (Location (x / blockSize, y / blockSize))
                )

handleKeys _ _ _ _ = return ()

updateAnimations :: Float -> World -> World
updateAnimations _ World {..} = World
    { renderInfo = renderInfo
        { mantaAnimation = updateBackgroundAnimation (mantaAnimation renderInfo)
        , playerAnimations = mapFromList $ map
            (updatePlayerInformation renderInfo)
            (maybeToList (player worldInfo) ++ toList (otherPlayers worldInfo))
        }
    , ..
    }

updateIO :: MonadIO m => Float -> WorldSTM -> m ()
updateIO diff (WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = updateAnimations diff state
    atomically $ writeTVar tvar newState
    return ()

updatePlayerInformation
    :: RenderInfo -> InGamePlayer -> (Player, PlayerAnimation)
updatePlayerInformation RenderInfo {..} InGamePlayer {..} =
    let
        maybePlayerAnim = lookup playerDescription playerAnimations
        playerAnim      = fromMaybe defaultPlayerAnimation maybePlayerAnim
        newAnim :: PlayerAnimation -> PlayerAnimation
        newAnim (PlayerRunningAnimation _)
            | abs (velocityX playerVelocity) >= 0.01 = updatePlayerAnimation
                playerAnim
            | otherwise = newPlayerIdleAnimation
        newAnim (PlayerIdleAnimation _)
            | abs (velocityX playerVelocity) <= 0.01
            = newPlayerRunnningAnimation
            | otherwise
            = updatePlayerAnimation playerAnim
    in
        (playerDescription, newAnim playerAnim)

setRunAction :: RunDirection -> PlayerAction -> PlayerAction
setRunAction dir PlayerAction {..} =
    PlayerAction {getAction = getAction { runAction = Just $ RunAction dir }}

setJumpAction :: PlayerAction -> PlayerAction
setJumpAction PlayerAction {..} =
    PlayerAction {getAction = getAction { jumpAction = Just JumpAction }}

setShootAction :: Angle -> PlayerAction -> PlayerAction
setShootAction angle PlayerAction {..} =
    PlayerAction {getAction = getAction { shootAction = Just angle }}

createShootAction :: Angle -> PlayerAction
createShootAction angle = PlayerAction $ Action Nothing Nothing (Just angle)

unsetRunAction :: PlayerAction -> PlayerAction
unsetRunAction PlayerAction {..} =
    PlayerAction {getAction = getAction { runAction = Nothing }}

unsetJumpAction :: PlayerAction -> PlayerAction
unsetJumpAction PlayerAction {..} =
    PlayerAction {getAction = getAction { jumpAction = Nothing }}

unsetShootAction :: PlayerAction -> PlayerAction
unsetShootAction PlayerAction {..} =
    PlayerAction {getAction = getAction { shootAction = Nothing }}

calculateAngle :: Location -> Location -> Angle
calculateAngle (Location (x1, y1)) (Location (x2, y2)) =
    Angle (atan2 (y2 - y1) (x2 - x1))

sendMessage :: TQueue ClientMessage -> ClientMessage -> STM ()
sendMessage receiveChannel message = writeTQueue receiveChannel message

sendMessageIO :: MonadIO m => TQueue ClientMessage -> ClientMessage -> m ()
sendMessageIO chan msg = atomically $ sendMessage chan msg

isEmpty :: PlayerAction -> Bool
isEmpty PlayerAction {..} =
    let Action {..} = getAction
    in  isNothing runAction && isNothing jumpAction && isNothing shootAction
