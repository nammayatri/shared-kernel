module Kernel.Tools.Metrics.ApiCategory
  ( ApiCategory (..),
    ApiCategoryConfig (..),
    mkApiCategoryConfig,
    categorizeHandler,
    apiCategoryToText,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import EulerHS.Prelude

data ApiCategory
  = Transactional
  | Config
  | Unclassified
  deriving (Show, Eq, Ord)

apiCategoryToText :: ApiCategory -> Text
apiCategoryToText Transactional = "transactional"
apiCategoryToText Config = "config"
apiCategoryToText Unclassified = "unclassified"

data ApiCategoryConfig = ApiCategoryConfig
  { transactionalRoutes :: Set Text,
    configRoutes :: Set Text
  }

mkApiCategoryConfig :: [Text] -> [Text] -> ApiCategoryConfig
mkApiCategoryConfig transactionalApiRoutes configApiRoutes =
  ApiCategoryConfig
    { transactionalRoutes = Set.fromList transactionalApiRoutes,
      configRoutes = Set.fromList configApiRoutes
    }

categorizeHandler :: ApiCategoryConfig -> Text -> ApiCategory
categorizeHandler cfg route
  | route `Set.member` transactionalRoutes cfg = Transactional
  | route `Set.member` configRoutes cfg = Config
  | otherwise = Unclassified
