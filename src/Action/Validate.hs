{-# LANGUAGE FlexibleContexts  #-}

module Action.Validate where

import Control.Lens
import Control.Monad.State

import Types.State

import Class.FM

import Data.FM.Types
import Data.FM.ProductConfiguration
import Data.FM.Utils

import Data.List.Split


validate :: (MonadState Env m, MonadFM m, MonadIO m) => m ()

validate = do

  fm <- gets (view fm)

  case fm of
    Nothing  -> liftIO $ putStrLn "please load a Feature Model to validate a derived product"

    (Just f) -> do
      liftIO $ putStrLn ""
      pprint (view featureTree f)
      liftIO $ putStrLn ""
      liftIO $ putStrLn "select the products you wish to validate, separated by commas. i.e: iris,security,sql,persistence,etc"
      prod <- liftIO $ getLine
      liftIO $ print $ splitOn "," prod
      b <- validateP f (prod :: ProductConfiguration)
      if b then
        liftIO $ putStrLn "valid product"
      else
        liftIO $ putStrLn "invalid product"

      liftIO $ print (view featureTree f)

  liftIO $ putStrLn ""
