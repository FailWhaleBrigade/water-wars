module WaterWars.Client.Render.Update where

import ClassyPrelude

import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import WaterWars.Client.Render.State

handleKeys :: Event -> World -> World
handleKeys (EventKey (Char c) Gloss.Down _ _) world@World {..}
    | c == 'a' = world {worldInfo = worldInfo { walkLeft = True }}
    | c == 'w' = world {worldInfo = worldInfo { jump = True }}
    | c == 's' = world {worldInfo = worldInfo { duck = True }}
    | c == 'd' = world {worldInfo = worldInfo { walkRight = True }}
handleKeys (EventKey (Char c) Gloss.Up _ _) world@World {..}
    | c == 'a' = world {worldInfo = worldInfo { walkLeft = False }}
    | c == 'w' = world {worldInfo = worldInfo { jump = False }}
    | c == 's' = world {worldInfo = worldInfo { duck = False }}
    | c == 'd' = world {worldInfo = worldInfo { walkRight = False }}
handleKeys _ world = world

handleKeysIO :: Event -> WorldSTM -> IO WorldSTM
handleKeysIO e world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = handleKeys e state
    writeTVar tvar newState
    return world

movePlayer :: Float -> World -> World
movePlayer seconds World {..} =
    let v                 = 50
        WorldInfo {..}    = worldInfo
        Location (dx, dy) = concatMap snd $ filter
            fst
            [ (walkLeft , Location (-v, 0))
            , (jump     , Location (0, v))
            , (walkRight, Location (v, 0))
            , (duck     , Location (0, -v))
            ]
        diffs       = Location (dx * seconds, dy * seconds)
        oldLocation = playerLocation player
    in  World
            { worldInfo = worldInfo
                { player = player { playerLocation = oldLocation ++ diffs }
                }
            , ..
            }

update :: Float -> World -> World
update = movePlayer

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = update diff state
    atomically $ writeTVar tvar newState
    return world
