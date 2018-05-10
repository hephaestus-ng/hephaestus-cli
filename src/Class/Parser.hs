{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Class.Parser where

import Text.Parsec
import Text.Parsec.String

import Control.Monad.State
import Control.Monad.Morph
import Data.Either.Combinators

import Types.Hephaestus
import Parser.XML.Main
import Parser.CK
import Data.FM.Types
import Data.SC.Asset
import Data.SC.Types
import Data.SPL



class Monad m => MonadParser m where
  runFMParser   :: String -> m (Either ParseError FeatureModel)
  runCKParser   :: String -> m (Either ParseError (ConfigurationKnowledge ComponentModel))
  purify        :: (Either ParseError a) -> m a


instance MonadParser Hephaestus where
  runFMParser path = do
    input <- liftIO $ readFile path
    res <- generalize $ runParserT parseFeatureIDE () path input
    return res

  runCKParser path = do
    input <- liftIO $ readFile path
    res <- generalize $ runParserT parseCK () path input
    return res

  purify r = return $ fromRight' r


loadFM :: (MonadParser m) => String -> m FeatureModel
loadFM f = do
  result <- runFMParser f
  result <- purify result
  return result


loadCK :: (MonadParser m) => String -> m (ConfigurationKnowledge ComponentModel)
loadCK f = do
  result <- runCKParser f
  result <- purify result
  return result
