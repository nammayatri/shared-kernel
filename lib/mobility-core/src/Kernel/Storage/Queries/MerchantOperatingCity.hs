{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Kernel.Storage.Queries.MerchantOperatingCity where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Beam.MerchantOperatingCity as Beam
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.App
import Kernel.Types.CacheFlow
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.MerchantOperatingCity
import qualified Sequelize as Se

createIfNotExist :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => MerchantOperatingCity -> m ()
createIfNotExist merchantOperatingCity = do
  city <- findAllWithKV [Se.Is Beam.city $ Se.Eq merchantOperatingCity.city] <&> listToMaybe
  case city of
    Just _ -> pure ()
    Nothing -> createWithKV merchantOperatingCity

findAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m [MerchantOperatingCity]
findAll = findAllWithKV [Se.Is Beam.city $ Se.Not $ Se.Eq ""]

instance FromTType' Beam.MerchantOperatingCity MerchantOperatingCity where
  fromTType' (Beam.MerchantOperatingCityT {..}) = do
    pure $
      Just
        MerchantOperatingCity
          { id = Id id,
            city = city,
            stdCode = stdCode
          }

instance ToTType' Beam.MerchantOperatingCity MerchantOperatingCity where
  toTType' (MerchantOperatingCity {..}) = do
    Beam.MerchantOperatingCityT
      { Beam.id = id.getId,
        Beam.city = city,
        Beam.stdCode = stdCode
      }
