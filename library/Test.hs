{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Test where

import ClassyPrelude

newtype X = X [Int] deriving newtype (MonoFunctor, MonoFoldable, Monoid, GrowingAppend, SemiSequence, MonoPointed)

type instance Element X = Int
