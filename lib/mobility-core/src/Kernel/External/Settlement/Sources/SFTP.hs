{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Sources.SFTP
  ( fetchSettlementFile,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Kernel.External.Encryption
import Kernel.External.Settlement.Types (SFTPConfig (..))
import Kernel.Prelude
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import qualified Prelude as P

fetchSettlementFile ::
  (EncFlow m r, MonadIO m) =>
  SFTPConfig ->
  Text ->
  m (Either Text LBS.ByteString)
fetchSettlementFile config fileName = do
  tmpDir <- liftIO getTemporaryDirectory
  let localPath = tmpDir </> T.unpack fileName
      remoteFile = T.unpack config.remotePath </> T.unpack fileName
      portArg = P.show config.port
  let userAtHost = T.unpack config.username <> "@" <> T.unpack config.host
  result <- case config.privateKeyPath of
    Just keyPath -> fetchWithKey keyPath userAtHost remoteFile localPath portArg
    Nothing -> fetchWithPassword config remoteFile localPath portArg
  case result of
    Right _ -> do
      contents <- liftIO $ LBS.readFile localPath
      liftIO $ removeFile localPath
      pure $ Right contents
    Left err -> pure $ Left err

fetchWithKey ::
  (MonadIO m) =>
  Text ->
  String ->
  String ->
  String ->
  String ->
  m (Either Text ())
fetchWithKey keyPath userAtHost remoteFile localPath portArg = do
  let sftpCmd = "get " <> remoteFile <> " " <> localPath <> "\nquit\n"
  (exitCode, _stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "sftp"
        [ "-i",
          T.unpack keyPath,
          "-P",
          portArg,
          "-o",
          "StrictHostKeyChecking=no",
          userAtHost
        ]
        sftpCmd
  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code ->
      pure $ Left $ "SFTP failed (exit " <> show code <> "): " <> T.pack stderr

fetchWithPassword ::
  (EncFlow m r, MonadIO m) =>
  SFTPConfig ->
  String ->
  String ->
  String ->
  m (Either Text ())
fetchWithPassword config remoteFile localPath portArg = do
  decryptedPassword <- decrypt config.password
  let scpTarget = T.unpack config.username <> "@" <> T.unpack config.host <> ":" <> remoteFile
  (exitCode, _stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "sshpass"
        ["-p", T.unpack decryptedPassword, "scp", "-P", portArg, "-o", "StrictHostKeyChecking=no", scpTarget, localPath]
        ""
  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code ->
      pure $ Left $ "SCP failed (exit " <> show code <> "): " <> T.pack stderr
