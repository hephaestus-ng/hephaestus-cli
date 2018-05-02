{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Shell where

import Types.Error
import Types.State

import Class.Parser

import Action.Help
import Action.Load

-- import Data.FM.Types
-- import Data.SPL

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens



shell :: IO ()
shell = welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      case cmd of
        "help"    -> help
        "load fm" -> load "fm"
        "load ck" -> load "ck"
      shellLoop




modifyFM :: (MonadState Env m) => m Env
modifyFM = do
  env <- get
  return env


readConfig :: (MonadReader env m, HasEnv env) => m String
readConfig = do
  cfg <- ask
  return (view path $ view fm cfg)


readHeph :: Hephaestus ()
readHeph = do
  cfg <- ask
  liftIO $ print cfg


newtype Hephaestus a = Hephaestus (ReaderT Env IO a)
  deriving (
    Functor, Applicative, Monad,
    MonadReader Env,
    MonadIO
  )
