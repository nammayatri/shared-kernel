{-# LANGUAGE DerivingStrategies #-}

-- | Optional per-request IO profiler.
--
-- When the environment variable @IO_PROFILE_DIR@ is set, every request gets an
-- 'IOProfiler' that appends one JSON line per recorded event to
-- @<dir>/<requestId>.jsonl@. Events come from two places:
--
--   * shared-kernel's own hooks (Redis, DB, HTTP, Kafka, in-mem cache, timed
--     steps, forks), recorded through 'Kernel.Types.Logging.withIOProfile';
--   * euler-hs, which reports every physical SQL / KV-Redis call to the
--     'EulerHS.Extra.IOProfile.IOProfileSink' built by 'profilerSink'.
--
-- The file is closed, and one @IO_PROFILE@ summary line is logged, once the
-- response has been sent and every forked thread of the request has finished.
module Kernel.Tools.IOProfiler
  ( IOProfiler,
    IOProfEvent (..),
    newIOProfiler,
    profilerSink,
    recordIOEvent,
    nowMs,
    forkStarted,
    forkFinished,
    markResponded,
  )
where

import qualified Control.Concurrent.MVar as MV
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlphaNum)
import qualified Data.IORef as IR
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified EulerHS.Extra.IOProfile as EIOP
import Kernel.Prelude
import System.Directory (createDirectoryIfMissing)
import qualified System.IO as IO

data ProfState = ProfState
  { psActiveForks :: !Int,
    psResponded :: !Bool,
    psFinished :: !Bool
  }

data IOProfiler = IOProfiler
  { ipRequestId :: Text,
    ipUrl :: Text,
    ipFilePath :: FilePath,
    -- | 'Nothing' once the file has been closed.
    ipHandle :: MV.MVar (Maybe IO.Handle),
    -- | "layer/kind" -> (call count, summed ms)
    ipTotals :: IR.IORef (M.Map Text (Int, Double)),
    ipState :: IR.IORef ProfState,
    ipLogSummary :: Text -> IO ()
  }

data IOProfEvent = IOProfEvent
  { layer :: Text,
    kind :: Text,
    op :: Text,
    target :: Text,
    startMs :: Double,
    endMs :: Double,
    thread :: Text,
    parentThread :: Maybe Text,
    ok :: Bool
  }

instance A.ToJSON IOProfEvent where
  toJSON e =
    A.object $
      [ "layer" A..= e.layer,
        "kind" A..= e.kind,
        "op" A..= e.op,
        "target" A..= e.target,
        "start" A..= e.startMs,
        "end" A..= e.endMs,
        "ms" A..= (e.endMs - e.startMs),
        "thread" A..= e.thread,
        "ok" A..= e.ok
      ]
        <> maybe [] (\p -> ["parent" A..= p]) e.parentThread

-- | Wall-clock milliseconds, the same clock euler-hs uses for its events.
nowMs :: IO Double
nowMs = (\t -> realToFrac t * 1000) <$> getPOSIXTime

newIOProfiler :: FilePath -> Text -> Text -> (Text -> IO ()) -> IO IOProfiler
newIOProfiler dir reqId reqUrl logSummary = do
  createDirectoryIfMissing True dir
  -- The request id comes from a client header: keep only safe file-name characters.
  let safeName = T.unpack . T.take 64 $ T.filter (\c -> isAlphaNum c || c == '-' || c == '_') reqId
      path = dir <> "/" <> (if null safeName then "unknown" else safeName) <> ".jsonl"
  h <- IO.openFile path IO.AppendMode
  IO.hSetEncoding h IO.utf8
  hVar <- MV.newMVar (Just h)
  totalsRef <- IR.newIORef M.empty
  stateRef <- IR.newIORef (ProfState 0 False False)
  startedAt <- nowMs
  let p =
        IOProfiler
          { ipRequestId = reqId,
            ipUrl = reqUrl,
            ipFilePath = path,
            ipHandle = hVar,
            ipTotals = totalsRef,
            ipState = stateRef,
            ipLogSummary = logSummary
          }
  writeLine p (A.object ["kind" A..= ("request" :: Text), "requestId" A..= reqId, "url" A..= reqUrl, "start" A..= startedAt])
  pure p

writeLine :: A.ToJSON a => IOProfiler -> a -> IO ()
writeLine p v = MV.withMVar p.ipHandle $ \case
  Nothing -> pure ()
  Just h -> BL.hPutStrLn h (A.encode v) >> IO.hFlush h

recordIOEvent :: IOProfiler -> IOProfEvent -> IO ()
recordIOEvent p e = do
  IR.atomicModifyIORef' p.ipTotals $ \m ->
    (M.insertWith add (e.layer <> "/" <> e.kind) (1, e.endMs - e.startMs) m, ())
  writeLine p e
  where
    add (c1, t1) (c2, t2) = (c1 + c2, t1 + t2)

-- | The sink handed to euler-hs: turns its physical-call events into profile lines.
profilerSink :: IOProfiler -> EIOP.IOProfileSink
profilerSink p = EIOP.IOProfileSink $ \e ->
  recordIOEvent p $
    IOProfEvent
      "physical"
      ( case e.ioKind of
          EIOP.SqlCall -> "sql"
          EIOP.KvRedisCall -> "kvredis"
      )
      ""
      e.ioTarget
      e.ioStartMs
      e.ioEndMs
      e.ioThread
      Nothing
      e.ioOk

forkStarted :: IOProfiler -> IO ()
forkStarted p = IR.atomicModifyIORef' p.ipState $ \s -> (s {psActiveForks = s.psActiveForks + 1}, ())

forkFinished :: IOProfiler -> IO ()
forkFinished p = do
  IR.atomicModifyIORef' p.ipState $ \s -> (s {psActiveForks = s.psActiveForks - 1}, ())
  finishIfDone p

markResponded :: IOProfiler -> IO ()
markResponded p = do
  IR.atomicModifyIORef' p.ipState $ \s -> (s {psResponded = True}, ())
  finishIfDone p

-- | Logs the summary and closes the file exactly once, when the response has
-- been sent and no forked thread is still running.
finishIfDone :: IOProfiler -> IO ()
finishIfDone p = do
  shouldFinish <- IR.atomicModifyIORef' p.ipState $ \s ->
    if s.psResponded && s.psActiveForks <= 0 && not s.psFinished
      then (s {psFinished = True}, True)
      else (s, False)
  when shouldFinish $ do
    ts <- IR.readIORef p.ipTotals
    let summary =
          A.object
            [ "requestId" A..= p.ipRequestId,
              "url" A..= p.ipUrl,
              "file" A..= p.ipFilePath,
              "totals" A..= M.map (\(c, ms) -> A.object ["count" A..= c, "ms" A..= ms]) ts
            ]
    p.ipLogSummary . decodeUtf8 . BL.toStrict $ A.encode summary
    MV.modifyMVar_ p.ipHandle $ \case
      Nothing -> pure Nothing
      Just h -> IO.hClose h >> pure Nothing
