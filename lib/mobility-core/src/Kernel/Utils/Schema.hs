{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Schema
  ( stripPrefixUnderscoreIfAny,
    untaggedValue,
    genericDeclareUnNamedSchema,
    objectWithSingleFieldParsing,
  )
where

import qualified Data.Aeson as A
import Data.OpenApi
import Data.OpenApi.Declare
import Data.OpenApi.Internal.Schema
import GHC.Generics
import Kernel.Prelude
import Kernel.Utils.Common (recursiveStrip)

stripPrefixUnderscoreIfAny :: SchemaOptions
stripPrefixUnderscoreIfAny =
  defaultSchemaOptions
    { fieldLabelModifier = recursiveStrip
    }

untaggedValue :: SchemaOptions
untaggedValue =
  defaultSchemaOptions
    { sumEncoding = A.UntaggedValue
    }

genericDeclareUnNamedSchema :: forall a. (Generic a, GToSchema (Rep a), Typeable a) => SchemaOptions -> Proxy a -> Declare (Definitions Schema) NamedSchema
genericDeclareUnNamedSchema opt prx = do
  res <- genericDeclareNamedSchema opt prx
  return $ res {_namedSchemaName = Nothing}

objectWithSingleFieldParsing :: (String -> String) -> SchemaOptions
objectWithSingleFieldParsing constructorMapping =
  defaultSchemaOptions
    { sumEncoding = A.ObjectWithSingleField,
      constructorTagModifier = constructorMapping
    }
