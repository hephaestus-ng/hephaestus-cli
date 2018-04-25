{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- extension used for deriving instances to hephaestus type

module Cli where

import Error
import Config
import Parser
-- import Shell
import Data.FM.Types
import Data.Tree

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

fmcfg    = FMConfig "fm-path" (FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) [])

envv     = Env "asset-path" fmcfg "ck-path"

splerr   = FeatureModelErr "invalid feature model"
asseterr = ParserTErr "invalid path"

herr     = HephSPLError splerr


type Command = String


-- Type class definitions
class Monad m => MonadLog m where
  shLog :: String -> m ()

instance MonadLog IO where
  shLog c = putStrLn c


class Monad m => MonadShell m where
  handleCommand :: Command -> m ()

-- Monadic operations behaviour when composing MonadShell with IO
instance MonadShell IO where
  handleCommand c = putStrLn c


shell :: (MonadShell m, MonadIO m, MonadLog m, MonadParser FeatureModel m)
         => m ()
shell = welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      shLog ("running command " ++ cmd)
      handleCommand cmd
      fm <- fmmain
      liftIO $ print fm
      shellLoop


testP :: (MonadParser FeatureModel m, MonadIO m) => m ()
testP = do
  fm <- fmmain
  liftIO $ print fm



-- Reading FM config through type class type constraints and lenses
-- We are saying that:
-- readConfig is an action which is a Monad that might read config of type env
-- from MonadReader
readConfig :: (MonadReader env m, HasFMConfig env) => m FMConfig
readConfig = do
  cfg <- ask
  return (view fmConfig cfg)


welcome :: (MonadIO m, MonadLog m) => m ()
welcome = do
  shLog ("---------------------------------------" )
  shLog ("---------- Hephaestus - Shell ---------" )
  shLog ("--" )
  shLog ("--" )
  shLog ("-- type 'help' to see available commands" )
  shLog ("--" )
  shLog ("-- loading your SPL:" )
  shLog ("--" )
  shLog ("-- 1- load fm <fm-path>" )
  shLog ("-- 2- load ck <ck-path>" )
  shLog ("-- 3- load asset ??" )
  shLog ("--" )



-- main :: (MonadIO m) => m ()
-- main = do
--   runReaderT (readConfig) hcfg

newtype Hephaestus a =
  Hephaestus {
    appHeph :: ReaderT Env IO a
    -- appHeph :: ReaderT HephConfig (ExceptT HephError IO) a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader Env,
    MonadIO
  )

main :: Hephaestus ()
main = undefined
