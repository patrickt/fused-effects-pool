{-# LANGUAGE DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Pool
  ( PoolC (PoolC)
  , runPool
  , module Control.Effect.Pool
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.Reader
import           Control.Effect.Exception
import           Control.Effect.Pool
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Pool
import           Data.Time.Clock
import           Numeric.Natural

newtype PoolC r m a = PoolC { drain :: ReaderC (Data.Pool.Pool r) m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)

instance forall r sig m . (MonadIO m, Has (Lift IO) sig m) => Algebra (Pool r :+: sig) (PoolC r m) where
  alg (L (With act k)) = do
    pool <- PoolC (ask @(Data.Pool.Pool r))
    let acquire = liftIO (Data.Pool.tryTakeResource pool)
    let release = liftIO . curry Data.Pool.destroyResource . swap
    done <- bracket acquire release (maybe (pure Nothing) (fmap Just . act . fst))
    k done

  alg (R other)        = PoolC (alg (R (handleCoercible other)))

runPool ::
  MonadIO m
  => IO r -- ^ Create a new resource.
  -> (r -> IO ()) -- ^ Destroy an existing resource.
  -> Natural -- ^ Must be greater than zero (we don't have a Cardinal type)
  -> NominalDiffTime -- ^ Amount of time for which an unused resource is kept open. Smallest acceptable value is 0.5.
  -> Int -- ^ Maximum number of resources to keep open per stripe
  -> PoolC r m a
  -> m a
runPool create destroy stripes open maxr (PoolC go) = do
  p <- liftIO (Data.Pool.createPool create destroy (fromIntegral stripes) open maxr)
  runReader p go <* liftIO (Data.Pool.destroyAllResources p)
