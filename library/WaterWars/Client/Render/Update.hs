module WaterWars.Client.Render.Update where

import           ClassyPrelude

import           Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import           WaterWars.Client.Render.State
import           WaterWars.Client.Render.Animation
import           WaterWars.Core.Game
import           WaterWars.Client.Render.Config

handleKeys :: Event -> World -> World
handleKeys (EventKey (Char c) Gloss.Down _ _) world@World {..}
    | c == 'a' = world { worldInfo = worldInfo { walkLeft = True } }
    | c == 'w' = world { worldInfo = worldInfo { jump = True } }
    | c == 's' = world { worldInfo = worldInfo { duck = True } }
    | c == 'd' = world { worldInfo = worldInfo { walkRight = True } }
handleKeys (EventKey (Char c) Gloss.Up _ _) world@World {..}
    | c == 'a' = world { worldInfo = worldInfo { walkLeft = False } }
    | c == 'w' = world { worldInfo = worldInfo { jump = False } }
    | c == 's' = world { worldInfo = worldInfo { duck = False } }
    | c == 'd' = world { worldInfo = worldInfo { walkRight = False } }
handleKeys (EventKey (SpecialKey KeyEnter) Gloss.Up _ _) world@World {..} =
    world { worldInfo = worldInfo { readyUp = True } }
handleKeys (EventKey (MouseButton LeftButton) Gloss.Up _ (x, y)) world@World {..}
    = world
        { worldInfo = worldInfo
            { shoot = Just $ Location (x / blockSize, y / blockSize)
            }
        }
handleKeys _ world = world

handleKeysIO :: Event -> WorldSTM -> IO WorldSTM
handleKeysIO e world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = handleKeys e state
    writeTVar tvar newState
    return world

advanceAnimations :: Float -> World -> World
advanceAnimations _ World {..} = World
    { renderInfo = renderInfo
        { mantaAnimation = updateBackgroundAnimation (mantaAnimation renderInfo)
        , playerAnimations = mapFromList $ map
            (updatePlayerInformation World {..})
            (toList $ getAllPlayers lastGameUpdate)
        , connectingAnimation = updateAnimation (connectingAnimation renderInfo)
        }
    , ..
    }

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = advanceAnimations diff state
    atomically $ writeTVar tvar newState
    return world

updatePlayerInformation :: World -> Player -> (Player, PlayerAnimation)
updatePlayerInformation World {..} player =
    let
        RenderInfo {..} = renderInfo
        GameState {..}  = gameStateUpdate lastGameUpdate

        maybePlayerAnim :: Maybe PlayerAnimation
        maybePlayerAnim = lookup player playerAnimations

        playerAnim :: PlayerAnimation
        playerAnim = fromMaybe defaultPlayerAnimation maybePlayerAnim

        nextAnimationStep :: InGamePlayer -> PlayerAnimation -> PlayerAnimation
        nextAnimationStep InGamePlayer {..} (PlayerRunningAnimation _)
            | abs (velocityX playerVelocity) >= 0.01 = updatePlayerAnimation
                playerAnim
            | otherwise = newPlayerIdleAnimation
        nextAnimationStep InGamePlayer {..} (PlayerIdleAnimation _)
            | abs (velocityX playerVelocity) <= 0.01
            = newPlayerRunnningAnimation
            | otherwise
            = updatePlayerAnimation playerAnim
        -- This should not happen
        nextAnimationStep _ (PlayerDeathAnimation _) = updatePlayerAnimation playerAnim
    in
        case
            find ((== player) . playerDescription)
                 (getInGamePlayers inGamePlayers)
        of
            Nothing           -> (player, newPlayerDeathAnimation)
            Just inGamePlayer -> (player, nextAnimationStep inGamePlayer playerAnim)


getAllPlayers :: ServerUpdate -> Seq Player
getAllPlayers serverUpdate =
    let GameState {..} = gameStateUpdate serverUpdate
    in  map playerDescription (getInGamePlayers inGamePlayers)
            ++ map deadPlayerDescription (getDeadPlayers gameDeadPlayers)
