module WaterWars.Core.DefaultGame where

import           ClassyPrelude
import           WaterWars.Core.Game
import           Data.Array.IArray

import           Data.List                                ( transpose )

defaultGameMap :: GameMap
defaultGameMap = GameMap
    { gameTerrain       = defaultTerrain
    , terrainDecoration = defaultDecoration
    }

defaultDecoration :: TerrainDecoration
defaultDecoration =
    TerrainDecoration $ listArray (BlockLocation (-8, -8), BlockLocation (8, 8)) []

defaultTerrain :: Terrain
defaultTerrain = Terrain
    { terrainBlocks = listArray
        (BlockLocation (-8, -8), BlockLocation (8, 8))
        (concat $ transpose $ reverse
            [ [SolidBlock TopLeftCorner]
            ++ replicate 15 (SolidBlock Ceil)
            ++ [SolidBlock TopRightCorner]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock BottomLeftCorner
              , SolidBlock Floor
              , SolidBlock BottomRightCorner
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock BottomLeftCorner
              , SolidBlock Floor
              , SolidBlock BottomRightCorner
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock Middle
              , SolidBlock Middle
              , SolidBlock Middle
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [ SolidBlock LeftWall
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , NoBlock
              , SolidBlock RightWall
              ]
            , [SolidBlock BottomLeftCorner]
            ++ replicate 15 (SolidBlock Floor)
            ++ [SolidBlock BottomRightCorner]
            ]
        )
    }

defaultGameState :: GameState
defaultGameState = GameState
    { inGamePlayers   = defaultInGamePlayers
    , gameProjectiles = defaultProjectiles
    , gameTicks = 0
    }

defaultInGamePlayers :: InGamePlayers
defaultInGamePlayers = InGamePlayers empty

{-
defaultInGamePlayer :: InGamePlayer
defaultInGamePlayer = InGamePlayer
    { playerDescription = defaultPlayer
    , playerLocation = Location (0.0, 0.0)
    , playerMaxHealth = 10
    , playerHealth = 10
    , playerViewDirection = 1
    , playerVelocity = VelocityVector 0 0
    }

defaultPlayer :: Player
defaultPlayer = Player "Player One"
-}
defaultProjectiles :: Projectiles
defaultProjectiles = Projectiles $ singleton defaultProjectile

defaultProjectile :: Projectile
defaultProjectile = newProjectileFromAngle (Location (1.0, 1.0)) (Angle 0)
