module WaterWars.Client.Render.Update where

import ClassyPrelude

import Graphics.Gloss.Interface.IO.Game

import WaterWars.Client.Render.State

handleKeys :: Event -> World -> World
handleKeys (EventKey (Char c) _ _ _) World {..}
    |
-- TODO: change variables such as `walkLeft` to True iff 'a' is down
-- TODO: on release key, update variables such as `walkLeft`
      c == 'a' = World
        { worldInfo = worldInfo
            { player = (player worldInfo) { playerVel = (-v, 0) }
            }
        , ..
        }
    | c == 'w' = World
        { worldInfo = worldInfo
            { player = (player worldInfo) { playerVel = (0, v) }
            }
        , ..
        }
    | c == 's' = World
        { worldInfo = worldInfo
            { player = (player worldInfo) { playerVel = (0, -v) }
            }
        , ..
        }
    | c == 'd' = World
        { worldInfo = worldInfo
            { player = (player worldInfo) { playerVel = (v, 0) }
            }
        , ..
        }
    where v = 10
handleKeys _ World {..} = World
    { worldInfo = worldInfo { player = (player worldInfo) { playerVel = (0, 0) }
                            }
    , ..
    }

handleKeysIO :: Event -> WorldSTM -> IO WorldSTM
handleKeysIO e world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = handleKeys e state
    writeTVar tvar newState
    return world

movePlayer :: Float -> World -> World
movePlayer seconds World {..} = World
    { worldInfo = worldInfo
        { player = (player worldInfo) { playerLoc = (x', y') }
        }
    , ..
    }
  where
    (x , y ) = playerLoc $ player worldInfo
    (vx, vy) = playerVel $ player worldInfo
    x'       = x + vx * seconds
    y'       = y + vy * seconds

update :: Float -> World -> World
update seconds = movePlayer seconds

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = update diff state
    atomically $ writeTVar tvar newState
    return world
