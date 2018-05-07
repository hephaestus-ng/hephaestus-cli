
{-# LANGUAGE FlexibleContexts #-}

module Shell where

import Types.Error
import Types.State

import Class.Parser

import Action.Help
import Action.Load

import Types.Hephaestus

import Data.FM.Types
import Data.SC.Types
import Data.SPL

import Control.Monad.State
import Control.Lens



shell :: ( MonadState Env m,
           MonadParser (ConfigurationKnowledge ComponentModel) m,
           MonadParser FeatureModel m,
           MonadIO m
         ) => m ()
shell = welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      env <- get
      case cmd of
        "help"    -> help

        "load fm" -> load "fm"
        "load ck" -> load "ck"

        "show env" -> liftIO $ print env

        "clear env" -> modify (\env -> env { _fm = Nothing, _asset = Nothing })
      shellLoop
