{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Parser where


import Text.Parsec
import Text.Parsec.String
import Control.Monad.Reader
import Data.Tree
import Data.Either.Combinators

import Commands

import Parser.XML.Main
import Parser.CK
import Data.FM.Types
import Data.SPL


type HephParser u a = ParsecT String u IO a



class Monad m => MonadParser a m where
  runHephParser :: String -> m (Either ParseError a)
  purify        :: (Either ParseError a) -> m a


instance MonadParser FeatureModel IO where
  runHephParser path = do
    result <- parseFromFile parseFeatureIDE path
    return result
  purify res = do
    return (fromRight' res)

instance MonadParser (ConfigurationKnowledge TestAsset) IO where
  runHephParser path = do
    result <- parseFromFile parseCK path
    return result
  purify res = do
    return (fromRight' res)



loadFM :: (MonadParser FeatureModel m) => String -> m FeatureModel
loadFM f = do
  result <- runHephParser f
  result <- purify result
  return result

loadFM2 :: String -> IO FeatureModel
loadFM2 f = do
  result <- runHephParser f
  result <- purify result
  return result

loadCK :: (MonadParser (ConfigurationKnowledge TestAsset) m, MonadIO m) => m (ConfigurationKnowledge TestAsset)
loadCK = do
  result <- runHephParser "test.ck"
  if (isRight result) then
    liftIO $ putStrLn "ck loaded."
  else
    liftIO $ putStrLn "ck error."
  result <- purify result
  return result












------------ Asset instance for testing CK parser

ck :: ConfigurationKnowledge TestAsset
ck = [(Ref "test", [])]

data TestAsset = TestAsset { nameT :: String } deriving Show

taParser :: Parsec String () (Transformation TestAsset)
taParser = string "setMessage(\"" >> many1 letter >>= \s -> string "\")" >> return (setMessage s)

setMessage :: String -> Transformation TestAsset
setMessage s _ (Product p) = Product $ p { nameT = s }

instance Asset TestAsset where
  initialize = Product $ TestAsset { nameT = "begin" }
  parserT    = taParser

------------
