module Kernel.Utils.Redact
  ( redactSecrets,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import qualified Text.Regex as TR

redactSecrets :: Text -> Text
redactSecrets t
  | any (`T.isInfixOf` lowered) markers = T.pack $ foldl' (\s (re, rep) -> TR.subRegex re s rep) (T.unpack t) rules
  | otherwise = t
  where
    lowered = T.toLower t

markers :: [Text]
markers =
  [ "aiza",
    "api-key",
    "apikey",
    "api_key",
    "api_secret",
    "key=",
    "\"key\"",
    "password",
    "passwd",
    "pwd=",
    "\"pwd\"",
    "secret",
    "token",
    "authorization",
    "bearer",
    "cookie",
    "session",
    "otp"
  ]

rules :: [(TR.Regex, String)]
rules =
  [ (TR.mkRegex "AIza[A-Za-z0-9_-]{20,}", "[REDACTED]"),
    ( ci "\\(\"([A-Za-z0-9_-]*(token|authorization|api-key|apikey|api_key|secret|password|cookie|session)[A-Za-z0-9_-]*)\",\"([^\"\\]|\\\\.)*\"\\)",
      "(\"\\1\",\"[REDACTED]\")"
    ),
    ( ci "\\(\"(password|passwd|pwd|secret|api_secret|apikey|api_key|api-key|key|token|access_token|client_secret)\",Just \"[^\"]*\"\\)",
      "(\"\\1\",Just \"[REDACTED]\")"
    ),
    ( ci "([?&](password|passwd|pwd|secret|api_secret|apikey|api_key|api-key|key|token|access_token|client_secret)=)[^&\" ]*",
      "\\1[REDACTED]"
    ),
    ( ci "\"(password|passwd|pwd|secret|api_secret|apikey|api_key|api-key|key|token|access_token|client_secret)\" *: *\"([^\"\\]|\\\\.)*\"",
      "\"\\1\":\"[REDACTED]\""
    ),
    (ci "Bearer [A-Za-z0-9._~+/=-]{8,}", "Bearer [REDACTED]"),
    (ci "(Invalid token: )[A-Za-z0-9._~+/=-]{8,}", "\\1[REDACTED]"),
    (ci "[0-9]{4,8}((%20|\\+| )is(%20|\\+| )your(%20|\\+| )OTP)", "****\\1")
  ]
  where
    ci p = TR.mkRegexWithOpts p True False
