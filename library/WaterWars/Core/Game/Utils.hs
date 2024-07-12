{-# LANGUAGE TypeOperators #-}
module WaterWars.Core.Game.Utils
    ( module WaterWars.Core.Game.Utils
    , module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Map
    , module WaterWars.Core.Game.Action
    )
where

import           ClassyPrelude                     hiding ( Reader
                                                          , ask
                                                          )


import           Effectful.State.Dynamic as State
import           Effectful.Reader.Static
import           Effectful

import           Data.Array.IArray

import           WaterWars.Core.Game.State
import           WaterWars.Core.Game.Map
import           WaterWars.Core.Game.Action
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.Game.Constants


-- TODO: refactor?
addInGamePlayer :: GameState -> InGamePlayer -> GameState
addInGamePlayer GameState {..} igp = GameState
    { inGamePlayers = InGamePlayers (igp `cons` getInGamePlayers inGamePlayers)
    , ..
    }

-- TODO: flip
removePlayer :: GameState -> Player -> GameState
removePlayer GameState {..} p = GameState
    { inGamePlayers = InGamePlayers
        (filter ((/= p) . playerDescription) $ getInGamePlayers inGamePlayers)
    , ..
    }

removePlayers :: Set Player -> GameState -> GameState
removePlayers ps gs@GameState {..} = gs
    { inGamePlayers = InGamePlayers
                          ( filter ((`notElem` ps) . playerDescription)
                          $ getInGamePlayers inGamePlayers
                          )
    }

moveLocation :: VelocityVector -> Location -> Location
moveLocation (VelocityVector dx dy) (Location (x, y)) =
    Location (x + dx, y + dy)

setPlayerCooldown :: InGamePlayer -> InGamePlayer
setPlayerCooldown player = player { playerShootCooldown = shootCooldown }

acceleratePlayer :: VelocityVector -> InGamePlayer -> InGamePlayer
acceleratePlayer v p@InGamePlayer {..} =
    setPlayerVelocity (playerVelocity ++ v) p

setPlayerVelocity :: VelocityVector -> InGamePlayer -> InGamePlayer
setPlayerVelocity v p = p { playerVelocity = v }

modifyPlayerVelocity
    :: (VelocityVector -> VelocityVector) -> InGamePlayer -> InGamePlayer
modifyPlayerVelocity f p = setPlayerVelocity (f $ playerVelocity p) p

setPlayerMovementState :: MovementState -> InGamePlayer -> InGamePlayer
setPlayerMovementState (location, velocity) p =
    p { playerLocation = location, playerVelocity = velocity }

playerMovementState :: InGamePlayer -> MovementState
playerMovementState InGamePlayer {..} = (playerLocation, playerVelocity)

getApproximateBlock :: Location -> BlockLocation
getApproximateBlock (Location (x, y)) = BlockLocation (round x, round y)


moveProjectile :: Projectile -> Projectile
moveProjectile projectile@Projectile {..} = projectile
    { projectileLocation = moveLocation projectileVelocity projectileLocation
    }

accelerateProjectile :: VelocityVector -> Projectile -> Projectile
accelerateProjectile v p@Projectile {..} =
    setProjectileVelocity (projectileVelocity ++ v) p

setProjectileVelocity :: VelocityVector -> Projectile -> Projectile
setProjectileVelocity v p = p { projectileVelocity = v }

modifyProjectileVelocity
    :: (VelocityVector -> VelocityVector) -> Projectile -> Projectile
modifyProjectileVelocity f p =
    setProjectileVelocity (f $ projectileVelocity p) p

newProjectileFromAngle :: InGamePlayer -> Angle -> Projectile
newProjectileFromAngle p@InGamePlayer {..} angle = Projectile
    (playerHeadLocation p)
    (velocityVectorFromPolar projectileSpeed angle)
    playerDescription

velocityVectorFromPolar :: Speed -> Angle -> VelocityVector
velocityVectorFromPolar (Speed speed) (Angle angle) =
    VelocityVector (speed * cos angle) (speed * sin angle)

addProjectile :: State GameState :> e => Projectile -> Eff e ()
addProjectile projectile = do
    Projectiles projectiles <- State.gets gameProjectiles
    let newProjectiles = projectile `cons` projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }

removeProjectiles :: State GameState :> e => Set Projectile -> Eff e ()
removeProjectiles ps = do
    Projectiles projectiles <- State.gets gameProjectiles
    let newProjectiles = filter (`notElem` ps) projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }

playerHeadLocation :: InGamePlayer -> Location
playerHeadLocation InGamePlayer { playerLocation = Location (x, y) } =
    Location (x, y + playerHeadHeight)

newDeadPlayer :: Integer -> InGamePlayer -> DeadPlayer
newDeadPlayer tick InGamePlayer{..} = DeadPlayer
    { deadPlayerDescription = playerDescription
    , deadPlayerLocation = playerLocation
    , playerDeathTick = tick
    }

addDeadPlayers :: State GameState :> e => [DeadPlayer] -> Eff e ()
addDeadPlayers ps = do
    DeadPlayers deadPlayers <- State.gets gameDeadPlayers
    let newDeadPlayers =  deadPlayers ++ fromList ps
    modify $ \s -> s { gameDeadPlayers = DeadPlayers newDeadPlayers }

angleForRunDirection :: RunDirection -> Angle
angleForRunDirection RunRight = 0
angleForRunDirection RunLeft  = pi

get4NeighborBlocks
    :: (BlockLocation, BlockLocation) -> BlockLocation -> [BlockLocation]
get4NeighborBlocks mapBounds (BlockLocation (x, y)) = filter
    (inRange mapBounds)
    [ BlockLocation (x + 1, y)
    , BlockLocation (x, y + 1)
    , BlockLocation (x - 1, y)
    , BlockLocation (x, y - 1)
    ]

blockLocationToLocation :: BlockLocation -> Location
blockLocationToLocation (BlockLocation (x, y)) =
    Location (fromIntegral x, fromIntegral y)

isSolidAt :: Terrain -> BlockLocation -> Bool
isSolidAt terrain location = inRange (terrainBounds terrain) location
    && isSolid (terrain `blockAt` location)

manhattanDistance :: BlockLocation -> BlockLocation -> Int
manhattanDistance (BlockLocation (x1, y1)) (BlockLocation (x2, y2)) =
    abs (x2 - x1) + abs (y2 - y1)

blockLeftX :: BlockLocation -> Float
blockLeftX (BlockLocation (x, _)) = fromIntegral x - 0.5
blockRightX :: BlockLocation -> Float
blockRightX (BlockLocation (x, _)) = fromIntegral x + 0.5
blockBotY :: BlockLocation -> Float
blockBotY (BlockLocation (_, y)) = fromIntegral y - 0.5
blockTopY :: BlockLocation -> Float
blockTopY (BlockLocation (_, y)) = fromIntegral y + 0.5
blockRangeX :: BlockLocation -> (Float, Float)
blockRangeX b = (blockLeftX b, blockRightX b)
blockRangeY :: BlockLocation -> (Float, Float)
blockRangeY b = (blockBotY b, blockTopY b)

asks :: Reader s :> r => (s -> a) -> Eff r a
asks f = map f ask
{-# INLINE asks #-}

gets :: State s :> r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}


mapMOverPlayers
    :: (State GameState :> e, Reader GameMap :> e)
    => (InGamePlayer -> Eff e InGamePlayer)
    -> Eff e ()
mapMOverPlayers mapping = do
    InGamePlayers players <- State.gets inGamePlayers
    newPlayers            <- mapM mapping players
    modify $ \s -> s { inGamePlayers = InGamePlayers newPlayers }

mapMOverProjectiles
    :: (State GameState :> e, Reader GameMap :> e)
    => (Projectile -> Eff e Projectile)
    -> Eff e ()
mapMOverProjectiles mapping = do
    Projectiles projectiles <- State.gets gameProjectiles
    newProjectiles          <- mapM mapping projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }

filterMOverProjectiles
    :: (State GameState :> e, Reader GameMap :> e)
    => (Projectile -> Eff e Bool)
    -> Eff e ()
filterMOverProjectiles predicate = do
    Projectiles projectiles <- State.gets gameProjectiles
    newProjectiles          <- filterM predicate projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }


-- utility functions for creation
newInGamePlayer :: Player -> Location -> InGamePlayer
newInGamePlayer player location = InGamePlayer
    { playerDescription      = player
    , playerLocation         = location
    , playerHealth           = 1
    , playerMaxHealth        = 1
    , playerLastRunDirection = RunLeft
    , playerVelocity         = VelocityVector 0 0
    , playerShootCooldown    = 0
    , playerWidth            = defaultPlayerWidth
    , playerHeight           = defaultPlayerHeight
    }

incrementGameTick :: GameState -> GameState
incrementGameTick s@GameState { gameTicks } = s { gameTicks = gameTicks + 1 }

-- TODO: test
minimumVector :: [VelocityVector] -> VelocityVector
minimumVector vs = fromMaybe (VelocityVector 0 0) $ do
    minX <- minimumByMay (compare `on` abs) . map velocityX $ vs
    minY <- minimumByMay (compare `on` abs) . map velocityY $ vs
    return $ VelocityVector minX minY
    -- TODO: what should happen if positive and negative vectors are given?

-- TODO: test
diffLocation :: Location -> Location -> VelocityVector
diffLocation (Location (x1, y1)) (Location (x2, y2)) =
    VelocityVector (x2 - x1) (y2 - y1)


getBlock :: Location -> Maybe BlockLocation
getBlock location =
    if location `isInsideBlock` block then Just block else Nothing
    where
        block = getApproximateBlock location

isInsideBlock :: Location -> BlockLocation -> Bool
isInsideBlock (Location (x, y)) block =
    blockRangeX block `hasInside` x && blockRangeY block `hasInside` y

hasInside :: Ord a => (a, a) -> a -> Bool
hasInside (l, u) x = l < x && x < u

containsInclusive :: Ord a => (a, a) -> a -> Bool
containsInclusive (l, u) x = l <= x && x <= u

boundedBy :: Ord a => (a, a) -> a -> a
boundedBy (l, u) x | x < l     = l
       | x > u     = u
       | otherwise = x
