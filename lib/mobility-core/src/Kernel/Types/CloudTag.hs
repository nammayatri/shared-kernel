module Kernel.Types.CloudTag
  ( cloudNameToCode,
    localCloudCode,
    cloudFallbackEnabled,
    tagLocalGuid,
    cloudCodeOfId,
  )
where

import EulerHS.KVConnector.CloudTag (cloudCodeOfGuid, tagGuidWithCloud)
import Kernel.Prelude
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

cloudNameToCode :: String -> Maybe Int
cloudNameToCode = \case
  "AWS" -> Just 0
  "GCP" -> Just 1
  "NAMMA" -> Just 2
  _ -> Nothing

localCloudCode :: Maybe Int
localCloudCode = unsafePerformIO $ (>>= cloudNameToCode) <$> lookupEnv "CLOUD_TYPE"
{-# NOINLINE localCloudCode #-}

cloudFallbackEnabled :: Bool
cloudFallbackEnabled = unsafePerformIO $ maybe False (`elem` ["true", "True", "TRUE", "1"]) <$> lookupEnv "KV_CLOUD_FALLBACK_ENABLED"
{-# NOINLINE cloudFallbackEnabled #-}

tagLocalGuid :: Text -> Text
tagLocalGuid guid = maybe guid (`tagGuidWithCloud` guid) localCloudCode

cloudCodeOfId :: Text -> Maybe Int
cloudCodeOfId = cloudCodeOfGuid
