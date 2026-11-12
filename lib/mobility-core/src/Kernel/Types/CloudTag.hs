module Kernel.Types.CloudTag
  ( cloudNameToCode,
    localCloudCode,
    cloudFallbackEnabled,
    kvMultiCloudReadEnabled,
    tagLocalGuid,
    cloudCodeOfId,
  )
where

import EulerHS.KVConnector.CloudTag (cloudCodeOfGuid, cloudNameToCode, localCloudCode, tagGuidWithCloud)
import Kernel.Prelude
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

cloudFallbackEnabled :: Bool
cloudFallbackEnabled = unsafePerformIO $ maybe False (`elem` ["true", "True", "TRUE", "1"]) <$> lookupEnv "KV_CLOUD_FALLBACK_ENABLED"
{-# NOINLINE cloudFallbackEnabled #-}

kvMultiCloudReadEnabled :: Bool
kvMultiCloudReadEnabled = unsafePerformIO $ maybe False (`elem` ["true", "True", "TRUE", "1"]) <$> lookupEnv "KV_MULTI_CLOUD_READ_ENABLED"
{-# NOINLINE kvMultiCloudReadEnabled #-}

tagLocalGuid :: Text -> Text
tagLocalGuid guid = maybe guid (`tagGuidWithCloud` guid) localCloudCode

cloudCodeOfId :: Text -> Maybe Int
cloudCodeOfId = cloudCodeOfGuid
