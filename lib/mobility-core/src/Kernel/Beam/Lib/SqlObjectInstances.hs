{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Beam.Lib.SqlObjectInstances where

import qualified Data.Aeson as A
import Kernel.External.Encryption (DbHash (..))
import Kernel.Prelude
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (convertToSQLObject))
import Text.Hex (encodeHex)

instance {-# OVERLAPPING #-} ToSQLObject DbHash where
  convertToSQLObject = SQLObjectValue . show . ("\\x" <>) . encodeHex . unDbHash

instance {-# OVERLAPPING #-} ToSQLObject A.Value where
  convertToSQLObject = SQLObjectValue . show . A.encode
