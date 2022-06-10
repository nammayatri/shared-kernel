{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Storage.Esqueleto.DTypeBuilder
  ( DTypeBuilder,
    liftToBuilder,
    buildDType,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Class
import Beckn.Types.Logging

newtype DTypeBuilder m t = DTypeBuilder
  { getTType :: m t
  }
  deriving newtype (Functor, Applicative, Monad)

liftToBuilder :: m t -> DTypeBuilder m t
liftToBuilder = DTypeBuilder

buildDType :: (MonadThrow m, Log m, QEntity t a) => DTypeBuilder m t -> m a
buildDType b = toResult =<< getTType b
