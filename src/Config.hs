{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens
import Control.Monad.Reader

import Data.FM.Types
import Data.SPL


--
-- LENS MANUAL DEFINITION FOR HEPHCONFIG VARIABLES
--


data Env =
  Env {
    _asset :: String,
    _fm    :: FMConfig,
    _ck    :: String
  } deriving (Show)


data FMConfig =
  FMConfig {
    _path     :: String,
    _parsedFM :: FeatureModel
  } deriving (Show)


class HasFMConfig t where
  fmConfig :: Lens' t FMConfig
  path     :: Lens' t String
  parsedFM :: Lens' t FeatureModel


instance HasFMConfig FMConfig where
  fmConfig = id
  path     =
    lens _path (\c a -> c { _path = a })
  parsedFM =
    lens _parsedFM (\c b -> c { _parsedFM = b })

instance HasFMConfig Env where
  fmConfig =
    lens _fm (\env fm -> env { _fm = fm})


class HasEnv t where
  env         :: Lens' t Env
  asset       :: Lens' t String
  fm          :: Lens' t FMConfig
  ck          :: Lens' t String

instance HasEnv Env where
  env   = id
  asset =
    lens _asset (\h a -> h { _asset = a })
  fm    =
    lens _fm (\h b -> h { _fm = b })
  ck    =
    lens _ck (\h c -> h { _ck = c })
