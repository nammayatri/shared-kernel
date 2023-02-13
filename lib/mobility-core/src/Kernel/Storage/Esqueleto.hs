module Kernel.Storage.Esqueleto
  ( module Types,
    module Functions,
    module Queries,
    module Logger,
    module Config,
    module SqlDB,
    module Class,
    module Reexport,
    module Transactionable,
    module DTypeBuilder,
    defaultQQ,
    defaultSqlSettings,
  )
where

import qualified Data.Text as T
import Database.Persist.Quasi.Internal
import Database.Persist.TH as Reexport
import EulerHS.Prelude hiding (Key)
import Kernel.Storage.Esqueleto.Class as Class (SolidType, TEntityKey (..), TType (..), extractSolidType)
import Kernel.Storage.Esqueleto.Config as Config (EsqDBFlow, EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.DTypeBuilder as DTypeBuilder (DTypeBuilder, buildDType)
import Kernel.Storage.Esqueleto.Functions as Functions
import Kernel.Storage.Esqueleto.Logger as Logger (LoggerIO)
import Kernel.Storage.Esqueleto.Queries as Queries
import Kernel.Storage.Esqueleto.SqlDB as SqlDB
import Kernel.Storage.Esqueleto.Transactionable as Transactionable
import Kernel.Storage.Esqueleto.Types as Types
import Kernel.Utils.Text
import Language.Haskell.TH.Quote

defaultQQ :: QuasiQuoter
defaultQQ =
  persistWith $
    upperCaseSettings
      { psToDBName = camelCaseToSnakeCase
      }

defaultSqlSettings :: MkPersistSettings
defaultSqlSettings =
  sqlSettings
    { mpsConstraintLabelModifier = \tableName fieldName ->
        if T.last tableName /= 'T'
          then "Table_name_must_end_with_T"
          else T.init tableName <> fieldName,
      mpsFieldLabelModifier = \_ fieldName -> fieldName
    }
