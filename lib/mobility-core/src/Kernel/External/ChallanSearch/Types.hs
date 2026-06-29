{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.ChallanSearch.Types
  ( module Kernel.External.ChallanSearch.Types,
  )
where

import qualified Data.Aeson as A
import Data.OpenApi hiding (email)
import Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import qualified Kernel.Prelude as KP

data ChallanSearchService = Signzy
  deriving (Show, Read, Eq, Ord, Generic, ToSchema, ToParamSchema)

-- remove the Array fallback once you add more constructors to ChallanSearchService type.
instance FromJSON ChallanSearchService where
  parseJSON (A.String val) = maybe (fail ("failed to parse String " <> show val <> " in ChallanSearchService type")) pure (KP.readMaybe $ T.unpack val)
  parseJSON (A.Array _) = pure Signzy
  parseJSON e = fail $ "unexpected type, expected String for ChallanSearchService" <> show e

instance ToJSON ChallanSearchService where
  toJSON = A.String . show

$(mkBeamInstancesForEnumAndList ''ChallanSearchService)
