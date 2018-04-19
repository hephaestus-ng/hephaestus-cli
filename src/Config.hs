{-# LANGUAGE FlexibleContexts #-}

module Config where

import Control.Lens


type Path        = String
type AssetConfig = Path
type CKConfig    = Path


data HephConfig =
  HephConfig {
    _assetConfig :: AssetConfig,
    _ckConfig    :: CKConfig
    -- _state       :: String
  } deriving (Show)


class HasHephConfig t where
  hephConfig  :: Lens' t HephConfig
  assetConfig :: Lens' t AssetConfig
  ckConfig    :: Lens' t CKConfig


instance HasHephConfig HephConfig where
  hephConfig  = id
  assetConfig =
    lens _assetConfig (\h a -> h { _assetConfig = a })
  ckConfig    =
    lens _ckConfig (\h b -> h { _ckConfig = b })
