{-# LANGUAGE FlexibleContexts  #-}

module Action.Load where

import Control.Monad.State

import Types.State

import Class.Parser


load :: ( MonadState Env m,
          MonadParser m,
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
