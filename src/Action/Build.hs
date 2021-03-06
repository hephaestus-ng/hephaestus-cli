{-# LANGUAGE FlexibleContexts  #-}

module Action.Build where

import Data.Maybe

import Control.Monad.State
import Control.Lens
import Types.State

import Class.Builder


buildProduct :: ( MonadState Env m, MonadBuilder m, MonadIO m) => m ()
buildProduct = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  building a product"
  liftIO $ putStrLn ""

  fm <- fmap fromJust $ gets (view fm)
  ck <- fmap fromJust $ gets (view ck)
  pc <- fmap fromJust $ gets (view pc)
  ab <- fmap fromJust $ gets (view ab)

  prod <- buildM fm ck pc ab
  modify (\env -> env { _prdct = Just prod })

  liftIO $ putStrLn ""
  liftIO $ print $ show prod
  liftIO $ putStrLn ""
