{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Storage.Esqueleto.DTypeBuilder
  ( DTypeBuilder,
    liftToBuilder,
    buildDType,
  )
where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Class
import Kernel.Types.Logging

newtype DTypeBuilder m t = DTypeBuilder
  { getTType :: m t
  }
  deriving newtype (Functor, Applicative, Monad, Log, MonadThrow)

liftToBuilder :: m t -> DTypeBuilder m t
liftToBuilder = DTypeBuilder

buildDType :: (MonadThrow m, Log m, QEntity t a) => DTypeBuilder m t -> m a
buildDType b = toResult =<< getTType b
