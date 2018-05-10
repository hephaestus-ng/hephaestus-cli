{-# LANGUAGE FlexibleContexts  #-}

module Action.Load where

import Control.Monad.State

import Types.State

import Data.FM.Types
import Data.SC.Types
import Data.SPL

import Class.Parser


load :: ( MonadState Env m,
          MonadParser FeatureModel m,
          MonadParser (ConfigurationKnowledge ComponentModel) m,
          MonadIO m
        ) => String -> m ()

load "fm" = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  load a feature model to hephaestus environment"
  liftIO $ putStrLn "  fm.xml path: "
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  res <- loadFM path
  modify (\env -> env { _fm = Just res })
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Feature Model was loaded to environment"
  liftIO $ putStrLn ""

load "ck" = do
  liftIO $ putStrLn ("  ")
  liftIO $ putStrLn ("  loading configuration knowledge to hephaestus environment")
  liftIO $ putStrLn ("  product.ck path:")
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  res <- loadCK path
  liftIO $ putStrLn "ck loaded"
