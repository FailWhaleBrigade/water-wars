{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-} -- TODO: ask someone

module WaterWars.Server.GameNg where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import WaterWars.Core.Physics
import Control.Eff.Exception
import Control.Eff.State.Strict
import Control.Eff


runGameTick :: GameState -> Map Player Action -> GameState
runGameTick gameState = run . flip execState gameState . gameTick

gameTick :: (Member (State GameState) e) => Map Player Action -> Eff e ()
gameTick actions = do
    moveProjectiles
    moveEntities actions
    return ()

-- |Moves all projectiles in the game. This is effectful since the movement
-- depends on the whole state
moveProjectiles :: (Member (State GameState) e) => Eff e ()
moveProjectiles = do
    Projectiles projectiles <- gets gameProjectiles
    newProjectiles          <- mapM moveProjectile projectiles
    modify $ \s -> s { gameProjectiles = Projectiles newProjectiles }
    return ()

moveProjectile :: (Member (State GameState) e) => Projectile -> Eff e Projectile
moveProjectile (projectile@Projectile {..}) = return projectile
    { projectileLocation = moveLocation projectileVelocity projectileLocation
    }

moveEntities :: Member (State GameState) e => Map Player Action -> Eff e ()
moveEntities actions = do
    Entities entities <- gets gameEntities
    newEntities       <- mapM (moveEntity actions) entities
    modify $ \s -> s { gameEntities = Entities newEntities }

moveEntity
    :: Member (State GameState) e => Map Player Action -> Entity -> Eff e Entity
moveEntity _ Npc = return Npc
moveEntity actions (EntityPlayer (player@InGamePlayer {..})) =
    let
        movedPlayerMay = do -- maybe monad
            Action a <- lookup playerDescription actions
            Run {..} <- find isRunAction a
            let v = runVelocityVector runDirection
            return player
                { playerLocation = moveLocation v playerLocation
                , playerVelocity = v
                }
    in  return . EntityPlayer $ fromMaybe player movedPlayerMay


data GameError = GameError deriving (Eq, Show)

throwError' :: (Member (Exc e) r) => e -> Eff r ()
throwError' = throwError
{-# INLINE throwError' #-}


gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}
-- TODO: implement with lenses??
