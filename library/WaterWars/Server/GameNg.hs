{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-} -- TODO: ask someone

module WaterWars.Server.GameNg where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import Control.Eff.Exception
import Control.Eff.State.Strict
import Control.Eff

gameTick
    :: (Member (State GameState) e, Member (Exc GameError) e)
    => Map Player Action
    -> Eff e ()
gameTick _ = do
    throwError' GameError
    return ()

-- |Moves all projectiles in the game. This is effectful since the movement
-- depends on the whole state
moveProjectiles :: (Member (State GameState) e) => Eff e ()
moveProjectiles = do
    Projectiles projectiles <- gets gameProjectiles
    newProjectiles          <- mapM moveProjectile projectiles
    modify (\s -> s { gameProjectiles = Projectiles newProjectiles })
    return ()

moveProjectile :: (Member (State GameState) e) => Projectile -> Eff e Projectile
moveProjectile (projectile@Projectile {..}) = return projectile
    { projectileLocation = moveLocation
        (projectileSpeed, projectileDirection)
        projectileLocation
    }

data GameError = GameError

throwError' :: (Member (Exc e) r) => e -> Eff r ()
throwError' = throwError
{-# INLINE throwError' #-}


gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = map f get
{-# INLINE gets #-}
-- TODO: implement with lenses??
