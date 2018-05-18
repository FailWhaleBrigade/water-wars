module WaterWars.Client.Render.Update where

import           ClassyPrelude

import           Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game
                                               as Gloss
import           WaterWars.Client.Render.State
import           WaterWars.Core.Game

handleKeys :: Event -> World -> World
handleKeys (EventKey (Char c) Gloss.Down _ _) world@World {..}
    | c == 'a' = world { worldInfo = worldInfo { walkLeft = True, lastDirection = RunLeft } }
    | c == 'w' = world { worldInfo = worldInfo { jump = True } }
    | c == 's' = world { worldInfo = worldInfo { duck = True } }
    | c == 'd' = world { worldInfo = worldInfo { walkRight = True, lastDirection = RunRight } }
handleKeys (EventKey (SpecialKey KeySpace) Gloss.Down _ _) world@World {..} =
    world { worldInfo = worldInfo { shoot = True } }
handleKeys (EventKey (Char c) Gloss.Up _ _) world@World {..}
    | c == 'a' = world { worldInfo = worldInfo { walkLeft = False } }
    | c == 'w' = world { worldInfo = worldInfo { jump = False } }
    | c == 's' = world { worldInfo = worldInfo { duck = False } }
    | c == 'd' = world { worldInfo = worldInfo { walkRight = False } }
    | c == ' ' = world { worldInfo = worldInfo { shoot = False } }
handleKeys (EventKey (SpecialKey KeySpace) Gloss.Up _ _) world@World {..} =
    world { worldInfo = worldInfo { shoot = False } }
handleKeys _ world = world

handleKeysIO :: Event -> WorldSTM -> IO WorldSTM
handleKeysIO e world@(WorldSTM tvar) = atomically $ do
    state <- readTVar tvar
    let newState = handleKeys e state
    writeTVar tvar newState
    return world

-- movePlayer :: Float -> World -> World
-- movePlayer seconds World {..} =
--     let v                 = 50
--         WorldInfo {..}    = worldInfo
--         Location (dx, dy) = concatMap snd $ filter
--             fst
--             [ (walkLeft , Location (-v, 0))
--             , (jump     , Location (0, v))
--             , (walkRight, Location (v, 0))
--             , (duck     , Location (0, -v))
--             ]
--         diffs       = Location (dx * seconds, dy * seconds)
--         oldLocation = playerLocation player
--     in  World
--             { worldInfo = worldInfo
--                 { player = player { playerLocation = oldLocation ++ diffs }
--                 }
--             , ..
--             }

updateAnimation :: Animation -> Animation
updateAnimation a@Animation {..} = if countDownTilNext == 0
    then a { picInd           = picInd + 1
           , location         = Location (x - 1, y)
           , countDownTilNext = countDownMax
           }
    else a { location         = Location (x - 1, y)
           , countDownTilNext = countDownTilNext - 1
           }
    where Location (x, y) = location

update :: Float -> World -> World
update _ World {..} =
    let worldAnimated = World
            { renderInfo = renderInfo
                { mantaAnimation = updateAnimation (mantaAnimation renderInfo)
                , playerAnimation = updateAnimation (playerAnimation renderInfo)
                , playerRunningAnimation = updateAnimation (playerRunningAnimation renderInfo)
                }
            , ..
            }
    in  worldAnimated

updateIO :: Float -> WorldSTM -> IO WorldSTM
updateIO diff world@(WorldSTM tvar) = do
    state <- readTVarIO tvar
    let newState = update diff state
    atomically $ writeTVar tvar newState
    return world
