{-# LANGUAGE FlexibleContexts  #-}

module Action.Export where

import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State

import Class.Builder

import Types.State


exportProduct :: (MonadState Env m, MonadBuilder m, MonadIO m) => m ()
exportProduct = do
  src  <- fmap fromJust $ gets (view src)
  trg  <- fmap fromJust $ gets (view target)
  prod <- fmap fromJust $ gets (view prdct)
  exportM src trg prod
  liftIO $ putStrLn ""
  liftIO $ putStrLn "builded product was exported with success"
  liftIO $ putStrLn ""


-- setPath :: (MonadState Env m, MonadIO m) => Int -> m ()
-- setPath 0 = do
--   liftIO $ putStrLn ""
--   liftIO $ putStrLn "specify a source code path"
--   liftIO $ putStrLn ""
--   src <- liftIO $ getLine
--   modify (\env -> env { _src = Just src })
-- setPath 1 = do
--   liftIO $ putStrLn ""
--   liftIO $ putStrLn "specify a target code path"
--   liftIO $ putStrLn ""
--   target <- liftIO $ getLine
--   modify (\env -> env { _target = Just target })
