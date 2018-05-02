module Types.Error where

import Control.Lens


splerr   = FeatureModelErr "invalid feature model"
asseterr = ParserTErr "invalid path"

herr     = HephSPLError splerr

data HephError = HephSPLError { hsplError :: SPLError }
               | HephAssetError { hassetError :: AssetError }
  deriving (Show)

instance AsAssetError HephError where
  assetError =
    prism' HephAssetError (\x ->
      case x of
        HephAssetError err -> Just err
        _                  -> Nothing)


instance AsSPLError HephError where
  splError =
    prism' HephSPLError (\x ->
      case x of
        HephSPLError err -> Just err
        _                -> Nothing)



-- AssetError with Prism implementation

type ParserErr = String

data AssetError = ParserTErr ParserErr | ParserAErr ParserErr
  deriving (Show)

class AsAssetError t where
  assetError :: Prism' t AssetError
  parserTErr :: Prism' t ParserErr
  parserAErr :: Prism' t ParserErr

instance AsAssetError AssetError where
  assetError = id
  parserTErr =
    prism' ParserTErr (\x ->
      case x of
        ParserTErr err -> Just err
        _              -> Nothing)
  parserAErr =
    prism' ParserAErr (\x ->
      case x of
        ParserAErr err -> Just err
        _              -> Nothing)


--SPLError with prism implementation

data SPLError = FeatureModelErr String | CfgKnowledgeErr String
  deriving (Show)

class AsSPLError t where
  splError        :: Prism' t SPLError
  featureModelErr :: Prism' t String
  cfgKnowledgeErr :: Prism' t String

instance AsSPLError SPLError where
  splError        = id
  featureModelErr =
    prism' FeatureModelErr $ (\x ->
      case x of
        FeatureModelErr err -> Just err
        _                   -> Nothing)
  cfgKnowledgeErr =
    prism' CfgKnowledgeErr $ (\x ->
      case x of
        CfgKnowledgeErr err -> Just err
        _                   -> Nothing)
