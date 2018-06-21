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

update :: Float -> World -> World
update _ World {..} = World
    { renderInfo = renderInfo
        { mantaAnimation = updateBackgroundAnimation (mantaAnimation renderInfo)
        , playerAnimations = mapFromList $ map
            (updatePlayerInformation renderInfo)
            (maybeToList (player worldInfo) ++ toList (otherPlayers worldInfo))
        }
    , ..
    }

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = update diff state
    atomically $ writeTVar tvar newState
    return world

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
