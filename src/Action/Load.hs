{-# LANGUAGE FlexibleContexts  #-}

module Action.Load where

import Control.Monad.State

import Types.State

import Class.Parser

import Data.List.Split

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

load "pc" = do
  liftIO $ putStrLn ("  ")
  liftIO $ putStrLn ("  loading product configuration to hephaestus environment")
  liftIO $ putStrLn ("  list of features:")
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  modify (\env -> env { _pc = Just (splitOn "," path) })
  liftIO $ putStrLn "ck loaded"

load "src" = do
  liftIO $ putStrLn ("  ")
  liftIO $ putStrLn ("  loading input source dir to hephaestus environment")
  liftIO $ putStrLn ("  src path:")
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  modify (\env -> env { _src = Just path })
  liftIO $ putStrLn "src loaded"
         
load "target" = do
  liftIO $ putStrLn ("  ")
  liftIO $ putStrLn ("  loading output dir to hephaestus environment")
  liftIO $ putStrLn ("  output path:")
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  modify (\env -> env { _target = Just path })
  liftIO $ putStrLn "target loaded"


