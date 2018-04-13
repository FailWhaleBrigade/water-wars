module WaterWars.Core.DefaultGame where

import ClassyPrelude
import WaterWars.Core.GameState
import Data.Array.IArray
import Data.List (cycle)

defaultGameMap :: GameMap
defaultGameMap = GameMap
  { gameTerrain = defaultTerrain
  , gamePlayers = singleton $ defaultPlayer
  }

defaultTerrain :: Terrain
defaultTerrain = Terrain
  { terrainBlocks = listArray (BlockLocation (-3, -3), BlockLocation (3, 3))
                              (cycle [SolidBlock, NoBlock])
  , terrainBackground = "Use Default Background"
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
