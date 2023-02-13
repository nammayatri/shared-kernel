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
