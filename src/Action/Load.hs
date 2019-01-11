{-# LANGUAGE FlexibleContexts  #-}

module Action.Load where

import Control.Monad.State
import Control.Lens

import Types.State

import Class.Parser
import Class.FM

import Data.List.Split

load :: ( MonadState Env m,
          MonadParser m,
          MonadFM m,
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
  liftIO $ putStrLn "  Feature Model was loaded to environment"
  liftIO $ putStrLn ""

load "ck" = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  loading configuration knowledge to hephaestus environment"
  liftIO $ putStrLn "  product.ck path:"
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  res <- loadCK path
  modify (\env -> env { _ck = Just res})
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  Configuration Knowledge was loaded to environment"
  liftIO $ putStrLn ""

load "asset-base" = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  loading asset base to hephaestus environment"
  liftIO $ putStrLn "  asset-base.txt path:"
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  res <- loadAB path
  modify (\env -> env { _ab = Just res})
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  Asset Base was loaded to environment"
  liftIO $ putStrLn ""

load "pc" = do
  fm <- gets (view fm)
  case fm of
    Nothing -> liftIO $ putStrLn "  please load a Feature Model to derive a product"
    Just f -> do
      liftIO $ putStrLn ""
      pprint f
      liftIO $ putStrLn ""
      liftIO $ putStrLn "  select the features that will be included in your product, separated by commas. i.e: iris,security,sql,persistence,etc"
      liftIO $ putStrLn ""
      prod <- liftIO $ getLine
      b <- validateP f (splitOn "," prod)
      if b then do
        liftIO $ putStrLn ""
        liftIO $ putStrLn "  valid product, loaded to environment"
        liftIO $ putStrLn ""
        modify (\env -> env { _pc = Just (splitOn "," prod) })
      else do
        liftIO $ putStrLn ""
        liftIO $ putStrLn "  invalid product, please choose a valid feature selection to load product configuration to environment"
        liftIO $ putStrLn ""

load "src" = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  loading input source dir to hephaestus environment"
  liftIO $ putStrLn "  src path:"
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  modify (\env -> env { _src = Just path })
  liftIO $ putStrLn "src loaded"

load "target" = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  loading output dir to hephaestus environment"
  liftIO $ putStrLn "  output path:"
  liftIO $ putStrLn ""
  path <- liftIO $ getLine
  modify (\env -> env { _target = Just path })
  liftIO $ putStrLn "target loaded"
