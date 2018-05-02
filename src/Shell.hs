{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- extension used for deriving instances to hephaestus type

module Shell where

import Types.Error
import Types.State

import Class.Parser

import Action.Help
import Action.Load

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




shell :: ( MonadParser FeatureModel m,
           MonadParser (ConfigurationKnowledge TestAsset) m,
           MonadIO m
         ) => m ()

shell = welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      -- fm <- fmEx
      case cmd of
        "help"    -> help
        "load fm" -> load "fm"
        "load ck" -> load "ck"
      shellLoop




ftree = Node (Feature "iris" BasicFeature Mandatory) [
        (Node (Feature "security" OrFeature Mandatory) []),
        (Node (Feature "persist" AltFeature Mandatory) [])
    ]


fmEx :: FeatureModel
fmEx = FeatureModel ftree []




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

main :: Hephaestus ()
main = undefined
