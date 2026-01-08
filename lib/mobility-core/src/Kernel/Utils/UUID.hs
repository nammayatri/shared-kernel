module Kernel.Utils.UUID where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import qualified Data.UUID as UUID
import Data.UUID.V5 (generateNamed, namespaceDNS)
import Kernel.Prelude

-- | Generates a static (deterministic) UUID v5 from a static text.
-- We use namespaceDNS as a base for the deterministic hash.
generateStaticUUID :: Text -> Text
generateStaticUUID staticText =
  let uuid = generateNamed namespaceDNS (B.unpack $ E.encodeUtf8 staticText)
   in UUID.toText uuid
