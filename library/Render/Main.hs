module Render.Main(main) where

import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Radius = Float
type Position = (Float, Float)

data MyGame = Game {
    playerLoc :: (Float, Float)
    , playerVel :: (Float, Float)
} deriving Show

initialState :: MyGame
initialState =
    Game {playerLoc = (0, -50) --the bottom middle of the field
                              , playerVel = (0, 0) -- not sure if we need velocity
                                                  }

window :: Display
window = FullScreen--InWindow "test" (200, 200) (10, 10)

fps :: Int
fps = 60

background :: Color
background = white

fieldWidth :: Float
fieldWidth = 150

fieldHeight :: Float
fieldHeight = 150

-- convert a game state into a picture
render :: MyGame -> Picture
render game = pictures [player, wall]
  where
    player =
        uncurry translate (playerLoc game) $ color playerColor $ circleSolid 20
    playerColor = red
    wall        = color wallColor $ rectangleWire fieldWidth fieldHeight
    wallColor   = black

handleKeys :: Event -> MyGame -> MyGame
handleKeys (EventKey (Char c) _ _ _) game
    | c == 'a' = game { playerVel = (-v, 0) }
    | c == 'w' = game { playerVel = (0, v) }
    | c == 's' = game { playerVel = (0, -v) }
    | c == 'd' = game { playerVel = (v, 0) }
    where v = 10
handleKeys _ game = game { playerVel = (0, 0) }

movePlayer :: Float -> MyGame -> MyGame
movePlayer seconds game = game { playerLoc = (x', y') }
  where
    (x , y ) = playerLoc game
    (vx, vy) = playerVel game
    x'       = x + vx * seconds
    y'       = y + vy * seconds

onWallCollision :: MyGame -> MyGame
onWallCollision game = game { playerVel = (vx', vy') }
  where
    radius   = 20
    (vx, vy) = playerVel game
    --TODO!!! this wont work i believe
    vy'      = if wallCollisionY (playerLoc game) radius then 0 else vy
    vx'      = if wallCollisionX (playerLoc game) radius then 0 else vx

-- given the position and radius of the ball, 
-- return whether a collision occured
wallCollisionY :: Position -> Radius -> Bool
wallCollisionY (_, y) radius = topCol || botCol
  where
    topCol = y - radius <= -fieldHeight / 2
    botCol = y + radius >= fieldHeight / 2

wallCollisionX :: Position -> Radius -> Bool
wallCollisionX (x, _) radius = leftCol || rightCol
  where
    leftCol  = x - radius <= -fieldWidth / 2
    rightCol = x + radius >= fieldWidth / 2

update :: Float -> MyGame -> MyGame
update seconds = movePlayer seconds . onWallCollision

main :: IO ()
main = play window background fps initialState render handleKeys update


