{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import Kernel.Storage.Esqueleto.Class as Class (FromTType (..), SolidType, TEntityKey (..), ToTType (..), extractSolidType)
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
    { mpsConstraintLabelModifier = \tableName_ fieldName_ ->
        if T.last tableName_ /= 'T'
          then "Table_name_must_end_with_T"
          else T.init tableName_ <> fieldName_,
      mpsFieldLabelModifier = \_ fieldName_ -> fieldName_
    }
