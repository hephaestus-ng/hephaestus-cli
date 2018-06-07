{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Class.Parser where

import Text.Parsec
import Text.Parsec.String

import Control.Monad.State
import Control.Monad.Morph

import Types.Hephaestus
import Parser.XML.Main
import Parser.CK
import Data.FM.Types
import Data.Assets
import Data.SPL



class Monad m => MonadParser m where
  runFMParser   :: String -> m (Either ParseError FeatureModel)
  runCKParser   :: (Asset a) => String -> m (Either ParseError (ConfigurationKnowledge a))
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

  purify r = case r of
    Right rs -> return rs
    Left err -> error $ show $ err


loadFM :: (MonadParser m) => String -> m FeatureModel
loadFM f = do
  result <- runFMParser f
  result <- purify result
  return result


loadCK :: (Asset a, MonadParser m) => String -> m (ConfigurationKnowledge a)
loadCK f = do
  result <- runCKParser f
  result <- purify result
  return result
