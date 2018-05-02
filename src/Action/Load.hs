{-# LANGUAGE FlexibleContexts  #-}

module Action.Load where

import Control.Monad.Reader

import Types.State

import Data.FM.Types
import Data.SPL

import Class.Parser


load :: (
          MonadParser FeatureModel m,
          MonadParser (ConfigurationKnowledge TestAsset) m,
          MonadIO m
        ) => String -> m ()

load "fm" = do
  liftIO $ putStrLn "  "
  liftIO $ putStrLn "  loading feature model to hephaestus environment"
  liftIO $ putStrLn "  fm.xml path: "
  path <- liftIO $ getLine
  res <- loadFM path
  liftIO $ print res

load "ck" = do
  liftIO $ putStrLn ("  ")
  liftIO $ putStrLn ("  loading configuration knowledge to hephaestus environment")
  liftIO $ putStrLn ("  product.ck path:")
  path <- liftIO $ getLine
  res <- loadCK path
  liftIO $ putStrLn "ck loaded"
