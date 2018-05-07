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



class Monad m => MonadParser a m where
  runHephParser :: String -> m (Either ParseError a)
  purify        :: (Either ParseError a) -> m a


instance MonadParser FeatureModel Hephaestus where
  runHephParser path = do
    input <- liftIO $ readFile path
    res <- generalize $ runParserT parseFeatureIDE () path input
    return res
  purify r = return $ fromRight' r

instance MonadParser (ConfigurationKnowledge ComponentModel) Hephaestus where
  runHephParser path = do
    input <- liftIO $ readFile path
    res <- generalize $ runParserT parseCK () path input
    return res
  purify r = return $ fromRight' r


loadFM :: (MonadParser FeatureModel m) => String -> m FeatureModel
loadFM f = do
  result <- runHephParser f
  result <- purify result
  return result


loadCK :: (MonadParser (ConfigurationKnowledge ComponentModel) m) => String -> m (ConfigurationKnowledge ComponentModel)
loadCK f = do
  result <- runHephParser f
  result <- purify result
  return result
