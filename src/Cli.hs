{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- extension used for deriving instances to hephaestus type

module Cli where

import Error
import Config
import Parser
import Shell


import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens as L


hcfg     = HephConfig "asset-path" "ck-path"

splerr   = FeatureModelErr "invalid feature model"
asseterr = ParserTErr "invalid path"

herr     = HephSPLError splerr




class Monad m => MonadShell m where
  getCommand :: m String
  runCommand :: String -> m ()


instance (HasHephConfig env, MonadIO m) => MonadShell (ReaderT env m) where
  getCommand = do
    getLine
  runCommand c = do
    liftIO $ putStrLn c

-- readConfig :: (MonadReader env m, HasHephConfig env, MonadIO m) => m ()
-- readConfig = do
--   cfg <- ask
--   liftIO $ putStrLn (view assetConfig cfg)

main :: (MonadShell m, MonadIO m) => m ()
main = do
  runReaderT (readConfig) hcfg














newtype Hephaestus a =
  Hephaestus {
    appHeph :: ReaderT HephConfig IO a
    -- appHeph :: ReaderT HephConfig (ExceptT HephError IO) a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader HephConfig,
    MonadIO
  )
