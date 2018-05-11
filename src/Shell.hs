
{-# LANGUAGE FlexibleContexts #-}

module Shell where

import Types.Error
import Types.State

import Class.Builder
import Class.Parser
import Class.FM

import Action.IO
import Action.Load
import Action.Validate
import Action.Build

import Types.Hephaestus

import Data.FM.Types
import Data.SC.Types
import Data.SPL

import Control.Monad.State
import Control.Lens


shell :: ( MonadState Env m,
           MonadFM m,
           MonadParser m,
           MonadBuilder m, 
           MonadIO m
         ) => m ()
shell = welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      env <- get
      case cmd of
        "help"             -> help

        "load fm"          -> load "fm"
        "load ck"          -> load "ck"
        "load pc"          -> load "pc"
        "load src"         -> load "src"
        "load target"      -> load "target"
        "build"            -> buildProduct


        "validate product" -> validate

        "show env"         -> liftIO $ print env
        "clear env"        -> clearEnv
      shellLoop




clearEnv :: (MonadState Env m) => m ()
clearEnv = modify (\env -> env { _fm = Nothing, _asset = Nothing })
