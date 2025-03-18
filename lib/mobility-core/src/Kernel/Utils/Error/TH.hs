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

  concat <$> forM typeCons \con -> do
    conName <- case con of
      NormalC conName _ -> pure conName
      RecC conName _ -> pure conName
      _ -> fail $ nameStr <> " should contain normal or record constructors"

    let conNameStr = nameBase conName
        conTypeName = TH.mkName $ conNameStr <> "T"
        conDataTypeDec = DataD [] conTypeName [] Nothing [] []
        typeInstanceDec = TySynInstD $ TySynEqn Nothing (ConT ''O.ErrorInfoType `AppT` ConT conTypeName) (ConT typeName)
        mkParamExp conType i = VarE 'OE.mkOpenApiExample `AppTypeE` conType `AppE` LitE (IntegerL $ toInteger i)

    funcBody <- case con of
      NormalC _ bangTypes -> do
        let paramsExp =
              (zip bangTypes [(1 :: Int) ..]) <&> \((_bang, conType), i) -> do
                mkParamExp conType i
        pure $ appendE $ ConE (TH.mkName conNameStr) NE.:| paramsExp
      RecC _ varBangTypes -> do
        let fieldsExp =
              (zip varBangTypes [(1 :: Int) ..]) <&> \((recName, _bang, conType), i) -> do
                (recName, mkParamExp conType i)
        pure $ RecConE (TH.mkName conNameStr) fieldsExp
      _ -> fail $ nameStr <> " should contain normal or record constructors"

    let funcInstanceDec = FunD 'O.mkErrExample [Clause [WildP] (NormalB funcBody) []]
        instanceDec = InstanceD Nothing [] (ConT ''O.HasErrorInfo `AppT` ConT conTypeName) [typeInstanceDec, funcInstanceDec]
    pure [conDataTypeDec, instanceDec]

appendE :: NE.NonEmpty TH.Exp -> TH.Exp
appendE = foldl1 AppE

mkOpenAPIErrorDebug :: Name -> Q [Dec]
mkOpenAPIErrorDebug name = do
  decs <- mkOpenAPIError name
  testFunc <- mkTestSplice decs
  pure $ decs <> testFunc
