module WaterWars.Core.DefaultGame where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.Terrain.Block
import Data.Array.IArray

import Data.List (transpose)

defaultGameMap :: GameMap
defaultGameMap = GameMap
  { gameTerrain = defaultTerrain
  , gamePlayers = singleton defaultPlayer
  }

defaultTerrain :: Terrain
defaultTerrain = Terrain
  { terrainBlocks = listArray (BlockLocation (-8, -8), BlockLocation (8, 8))
                              (concat $ transpose $ reverse
                              [ [SolidBlock TopLeftCorner] ++ replicate 15 (SolidBlock Ceil) ++ [SolidBlock TopRightCorner]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock BottomLeftCorner, SolidBlock Floor, SolidBlock BottomRightCorner, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, SolidBlock BottomLeftCorner, SolidBlock Floor, SolidBlock BottomRightCorner, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, SolidBlock RightWall]
                              , [SolidBlock BottomLeftCorner] ++ replicate 15 (SolidBlock Floor) ++ [SolidBlock BottomRightCorner]
                              ] 
                              )
  , terrainBackground = "default"
  }

defaultGameState :: GameState
defaultGameState = GameState
    { gameEntities = defaultEntities
    , gameProjectiles = defaultProjectiles
    }

defaultEntities :: Entities
defaultEntities = Entities $ fromList [ defaultEntityPlayer ]

defaultEntityPlayer :: Entity
defaultEntityPlayer = EntityPlayer defaultInGamePlayer

defaultInGamePlayer :: InGamePlayer
defaultInGamePlayer = InGamePlayer
    { playerDesciption = defaultPlayer
    , playerLocation = Location (0.0, 0.0)
    , playerMaxHealth = 10
    , playerHealth = 10
    , playerViewDirection = 1
    , playerMoveDirection = 0
    }

defaultPlayer :: Player
defaultPlayer = Player "Player One"

defaultProjectiles :: Projectiles
defaultProjectiles = Projectiles $ singleton defaultProjectile

defaultProjectile :: Projectile
defaultProjectile = Projectile
    { projectileLocation = Location (1.0, 1.0)
    , projectileSpeed = 1
    , projectileDirection = 0
    }
