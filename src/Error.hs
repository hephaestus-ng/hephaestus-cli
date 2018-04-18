module Error where

import Control.Lens



data HephError = HephSPLError { hsplError :: SPLError }
               | HephAssetError { hassetError :: AssetError }
               | Unknown3

instance AsAssetError HephError where
  assetError =
    prism HephAssetError (\x ->
      case x of
        HephAssetError err -> Right err
        _                  -> Left Unknown3)


-- AssetError with Prism implementation
type PathErr   = String
type FormatErr = String

data ParserErr = PathErr | FormatErr

data AssetError = ParserTErr ParserErr | ParserAErr ParserErr | Unknown

class AsAssetError t where
  assetError :: Prism' t AssetError
  parserTErr :: Prism' t ParserErr
  parserAErr :: Prism' t ParserErr

instance AsAssetError AssetError where
  assetError = id
  parserTErr =
    prism ParserTErr (\x ->
      case x of
        ParserTErr err -> Right err
        _              -> Left Unknown)
  parserAErr =
    prism ParserAErr (\x ->
      case x of
        ParserAErr err -> Right err
        _              -> Left Unknown)


--SPLError with prism implementation

data SPLError = FeatureModelErr String | CfgKnowledgeErr String | Unknown2

class AsSPLError t where
  splError        :: Prism' t SPLError
  featureModelErr :: Prism' t SPLError
  cfgKnowledgeErr :: Prism' t SPLError

instance AsSPLError SPLError where
  splError        = id
  featureModelErr =
    prism FeatureModelErr $ (\x ->
      case x of
        FeatureModelErr err -> Right err
        _                   -> Left Unknown2)
  cfgKnowledgeErr =
    prism CfgKnowledgeErr $ (\x ->
      case x of
        CfgKnowledgeErr err -> Right err
        _                   -> Left Unknown2)
