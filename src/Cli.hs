{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- extension used for deriving instances to hephaestus type

module Cli where

import Error
import Config
import Parser
import Commands
import Text.Parsec

import Data.FM.Types
import Data.SPL
import Data.Tree
import Data.Either.Combinators

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

fmcfg    = FM "fm-path" (FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) [])

envv     = Env "asset-path" fmcfg "ck-path"

splerr   = FeatureModelErr "invalid feature model"
asseterr = ParserTErr "invalid path"

herr     = HephSPLError splerr


class Monad m => MonadLog m where
  shLog :: String -> m ()
  shGet :: m String

instance MonadLog IO where
  shLog c = putStrLn c >> putStrLn ""
  shGet   = getLine


shell :: (MonadParser FeatureModel m,
         MonadParser (ConfigurationKnowledge TestAsset) m,
         MonadLog m, MonadIO m) => m ()
shell = welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      case cmd of
        "help"    -> help
        "load fm" -> load "fm"
        "load ck" -> load "ck"
      shellLoop


load :: (MonadParser FeatureModel m,
         MonadParser (ConfigurationKnowledge TestAsset) m,
         MonadLog m, MonadIO m) => String -> m ()
load "fm" = do
  shLog ("& loading feature model to hephaestus environment")
  shLog ("& fm.xml path:")
  path <- shGet
  res <- loadFM path
  liftIO $ print res

load "ck" = do
  shLog ("  loading configuration knowledge to hephaestus environment")
  shLog ("  product.ck path:")
  path <- shGet
  res <- loadCK path
  liftIO $ print res


readConfig :: (MonadReader env m, HasFM env) => m FM
readConfig = do
  cfg <- ask
  return (view fmConfig cfg)


welcome :: (MonadIO m, MonadLog m) => m ()
welcome = do
  shLog ("---------------------------------------" )
  shLog ("---------- Hephaestus - Shell ---------" )
  shLog ("-- type 'help' to see available commands" )
  shLog ("-- load your SPL:" )
  shLog ("   1- load fm <fm-path>" )
  shLog ("   2- load ck <ck-path>" )
  shLog ("   3- load asset ??" )




newtype Hephaestus a =
  Hephaestus {
    appHeph :: ReaderT Env IO a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader Env,
    MonadIO
  )

main :: Hephaestus ()
main = undefined
