 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Exit where

import System.Exit (ExitCode (..))

exitSuccess :: ExitCode
exitSuccess = ExitSuccess

exitAuthManagerPrepFailure :: ExitCode
exitAuthManagerPrepFailure = ExitFailure 1

exitDBConnPrepFailure :: ExitCode
exitDBConnPrepFailure = ExitFailure 2

exitDBMigrationFailure :: ExitCode
exitDBMigrationFailure = ExitFailure 3

exitLoadAllProvidersFailure :: ExitCode
exitLoadAllProvidersFailure = ExitFailure 4

exitRedisConnPrepFailure :: ExitCode
exitRedisConnPrepFailure = ExitFailure 5

exitConnCheckFailure :: ExitCode
exitConnCheckFailure = ExitFailure 8

exitBuildingAppEnvFailure :: ExitCode
exitBuildingAppEnvFailure = ExitFailure 9
