{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Class.Loader where

import Text.Parsec
import Text.Parsec.String

import Control.Monad.State

import Class.Parser

import Types.State
import Data.FM.Types
import Data.SPL


class Monad m => MonadLoader m where
  runLoader :: String -> m ()


instance MonadLoader IO where
  runLoader path = load2 path


load2 :: (MonadParser FeatureModel m) => String -> m FeatureModel
load2 f = do
  result <- runHephParser f
  return $ res


hParseFM :: String -> IO FeatureModel
hParseFM p = do
  result <- parseFromFile parseFeatureIDE p
  return $ fromRight' result

-- loadCK :: (Monad m) => String -> m (ConfigurationKnowledge TestAsset)
-- loadCK f = do
--   result <- parseFromFile parseCK f
--   return $ fromRight' result
