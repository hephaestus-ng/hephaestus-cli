{-# LANGUAGE FlexibleContexts #-}

module Config where

import Control.Lens
import Control.Monad.Reader

import Data.FM.Types
import Data.SPL


--
-- This monad declaration is used to regain purity in the functions that will
-- call modifyAssetCfg, and functions that perform IO Actions in general
--
-- class Monad m => MonadHephConfig m where
--   -- readConfig     :: m HephConfig
--   modifyAssetCfg :: String -> m ()
--
-- instance (HasHephConfig env, MonadIO m) => MonadHephConfig (ReaderT env m) where
--   modifyAssetCfg s = do
--     cfg <- ask
--     liftIO $ putStrLn (view fm cfg)



--
-- LENS MANUAL DEFINITION FOR HEPHCONFIG VARIABLES
--
data HephConfig =
  HephConfig {
    _asset :: String,
    _fm    :: String,
    _ck    :: String
  } deriving (Show)


class HasHephConfig t where
  hephConfig  :: Lens' t HephConfig
  asset       :: Lens' t String
  fm          :: Lens' t String
  ck          :: Lens' t String


instance HasHephConfig HephConfig where
  hephConfig  = id
  asset =
    lens _asset (\h a -> h { _asset = a })
  fm    =
    lens _fm (\h b -> h { _fm = b })
  ck    =
    lens _ck (\h c -> h { _ck = c })
