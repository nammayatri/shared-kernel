-- TODO remove
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.Error.TH (mkOpenAPIError, mkOpenAPIErrorDebug) where

import qualified Data.List.NonEmpty as NE
import Kernel.Prelude
import qualified Kernel.Utils.Error.OpenApi as O
import qualified Kernel.Utils.Error.OpenApi.Example as OE
import Kernel.Utils.TH (mkTestSplice)
import Language.Haskell.TH as TH

mkOpenAPIError :: Name -> Q [Dec]
mkOpenAPIError name = do
  let nameStr = nameBase name
  mbTypeName <- lookupTypeName nameStr
  typeName <- case mbTypeName of
    Nothing -> fail $ nameStr <> " should be type name"
    Just n -> pure n
  errorTypeInfo <- reify typeName

  typeCons <- case errorTypeInfo of
    TyConI dec -> case dec of
      DataD _ _ _ _ cons _ -> pure cons
      NewtypeD _ _ _ _ con _ -> pure [con]
      _ -> fail $ nameStr <> " should be data type or newtype"
    _ -> fail $ nameStr <> " should be type name"

  concat <$> forM typeCons \con -> case con of
    NormalC conName conTypes -> do
      paramsExp <- forM (zip conTypes [(1 :: Int) ..]) \((_bang, conType), i) -> do
        pure $ VarE 'OE.mkOpenApiExample `AppTypeE` conType `AppE` LitE (IntegerL $ toInteger i)
      let conNameStr = nameBase conName
          conTypeName = TH.mkName $ conNameStr <> "T"
          conDataTypeDec = DataD [] conTypeName [] Nothing [] []
          typeInstanceDec = TySynInstD $ TySynEqn Nothing (ConT ''O.ErrorInfoType `AppT` ConT conTypeName) (ConT typeName)
          funcInstanceDec = FunD 'O.mkErrExample [Clause [WildP] (NormalB $ appendE $ ConE (TH.mkName conNameStr) NE.:| paramsExp) []]
          instanceDec = InstanceD Nothing [] (ConT ''O.HasErrorInfo `AppT` ConT conTypeName) [typeInstanceDec, funcInstanceDec]
      pure [conDataTypeDec, instanceDec]
    _ -> fail $ nameStr <> " should contain normal constructors"

appendE :: NE.NonEmpty TH.Exp -> TH.Exp
appendE = foldl1 AppE

mkOpenAPIErrorDebug :: Name -> Q [Dec]
mkOpenAPIErrorDebug name = do
  decs <- mkOpenAPIError name
  testFunc <- mkTestSplice decs
  pure $ decs <> testFunc
