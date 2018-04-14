module WaterWars.Client.Render.Update where

import ClassyPrelude

import Graphics.Gloss.Interface.IO.Game

import WaterWars.Client.Render.State

handleKeys :: Event -> World -> World
handleKeys (EventKey (Char c) _ _ _) world
    | c == 'a'  = world { player = (player world) { playerVel = (-v, 0) } }
    | c == 'w'  = world { player = (player world) { playerVel = (0, v) } }
    | c == 's'  = world { player = (player world) { playerVel = (0, -v) } }
    | c == 'd'  = world { player = (player world) { playerVel = (v, 0) } }
    where v = 10
handleKeys _ world = world { player = (player world) { playerVel = (0, 0) } }

handleKeysIO :: Event -> WorldSTM -> IO WorldSTM
handleKeysIO e world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = handleKeys e state
    writeTVar tvar newState
    return world

movePlayer :: Float -> World -> World
movePlayer seconds world = world { player = (player world) { playerLoc = (x', y') } }
  where
    (x , y ) = playerLoc $ player world
    (vx, vy) = playerVel $ player world
    x'       = x + vx * seconds
    y'       = y + vy * seconds

update :: Float -> World -> World
update seconds = movePlayer seconds

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = update diff state
    writeTVar tvar newState
    return world


