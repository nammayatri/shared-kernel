{-# LANGUAGE DerivingStrategies #-}

module Kernel.Beam.Types where

import qualified Database.Beam.Postgres as BP
import EulerHS.Prelude hiding (getOption)
import EulerHS.Types (DBConfig, OptionEntity)

data PsqlDbCfg = PsqlDbCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PsqlDbCfg (DBConfig BP.Pg)

data PsqlDbCfgR1 = PsqlDbCfgR1
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PsqlDbCfgR1 (DBConfig BP.Pg)

newtype DatabaseError = DatabaseError
  { errorMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance Exception DatabaseError
