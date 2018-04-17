module WaterWars.Client.Render.Update where

import ClassyPrelude

import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import WaterWars.Client.Render.State

handleKeys :: Event -> World -> World
handleKeys (EventKey (Char c) Gloss.Down _ _) World {..}
    | c == 'a' = World {worldInfo = worldInfo { walkLeft = True }, ..}
    | c == 'w' = World {worldInfo = worldInfo { jump = True }, ..}
    | c == 's' = World {..}
    | c == 'd' = World {worldInfo = worldInfo { walkRight = True }, ..}
handleKeys (EventKey (Char c) Gloss.Up _ _) World {..}
    | c == 'a' = World {worldInfo = worldInfo { walkLeft = False }, ..}
    | c == 'w' = World {worldInfo = worldInfo { jump = False }, ..}
    | c == 's' = World {..}
    | c == 'd' = World {worldInfo = worldInfo { walkRight = False }, ..}
handleKeys _ World {..} = World {..}

handleKeysIO :: Event -> WorldSTM -> IO WorldSTM
handleKeysIO e world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = handleKeys e state
    writeTVar tvar newState
    return world

movePlayer :: Float -> World -> World
movePlayer seconds World {..} =
    let v              = 10
        WorldInfo {..} = worldInfo
        diffs          = concatMap snd $ filter
            fst
            [ (walkLeft , Location (-v, 0))
            , (jump     , Location (0, v))
            , (walkRight, Location (v, 0))
            ]
        oldLocation = playerLocation player
    in  World
            { worldInfo = worldInfo
                { player = player { playerLocation = oldLocation ++ diffs }
                }
            , ..
            }

update :: Float -> World -> World
update seconds = movePlayer seconds

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = update diff state
    atomically $ writeTVar tvar newState
    return world
