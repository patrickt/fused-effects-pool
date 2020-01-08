{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Control.Effect.Pool
  ( Pool (..)
  , with
  ) where

import Data.Kind (Type)
import Control.Algebra

data Pool a (m :: Type -> Type) k
  = forall b . With (a -> m b) (Maybe b -> m k)

instance HFunctor (Pool r) where
  hmap f (With act k) = With (f . act) (f . k)

instance Effect (Pool a) where
  thread ctx handler (With act k)
    = With
      (\a -> handler (act a <$ ctx))
      (\case
          Just v -> handler (fmap k (fmap Just v))
          Nothing -> handler (k Nothing <$ ctx))

with :: (Has (Pool r) sig m) => (r -> m b) -> m (Maybe b)
with act = send (With act pure)
