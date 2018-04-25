{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Parser where


import Text.Parsec
import Text.Parsec.String
import Control.Monad.Reader
import Data.Tree
import Data.Either.Combinators

import Parser.XML.Main
import Parser.CK
import Data.FM.Types
import Data.SPL



type Result a = (String, a)


type HephParser u a = ParsecT String u IO a


class Monad m => MonadParser a m where
  runHephParser :: String -> m a


-- a intenção é parametrizar a função de rodar um parser do Hephaestus
-- pra indicar seu tipo de retorno

instance MonadParser FeatureModel IO where
  runHephParser path = do
    result <- parseFromFile parseFeatureIDE path
    if (isRight result) then
      return (fromRight' result)
    else
      return $ FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) []


instance MonadParser (ConfigurationKnowledge TestAsset) IO where
  runHephParser path = do
    result <- parseFromFile parseCK path
    liftIO $ putStrLn "bora"
    if (isRight result) then
      do
        liftIO $ putStrLn "bora2"
        return (fromRight' result)
    else
      return $ ck


-- class HasParserFM a where
--   getParserFM :: a -> String -> m FeatureModel
--
-- instance (MonadParser FeatureModel m) => HasParserFM m where
--   getParserFM path = runHephParser path

fmmain :: (MonadParser FeatureModel m) => m FeatureModel
fmmain = runHephParser "fm.ide"

ckmain :: (MonadParser (ConfigurationKnowledge TestAsset) m) => m (ConfigurationKnowledge TestAsset)
ckmain = runHephParser "test.ck"












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



------------ FUNC DEFINITIONS WITHOUT MONADIC TYPECLASSES

runPP s = runParserT parseCommand () "" s

parseCommand :: HephParser u FeatureModel
parseCommand =
  (parseLoad)

parseLoad :: HephParser u FeatureModel
parseLoad =
  string "load" >> many space >> many1 letter >>= \t ->
    case t of
      "fm" -> many space >> many1 anyChar >>= \path -> runParserFM path
      -- "ck" -> liftIO $ putStrLn "ck222"


runParserFM :: String -> HephParser u FeatureModel
runParserFM path = do
  result <- lift $ parseFromFile parseFeatureIDE path
  if (isRight result) then
    return (fromRight' result)
  else
    return $ FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) []


parseBuild :: HephParser u FeatureModel
parseBuild = undefined
