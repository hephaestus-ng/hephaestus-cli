{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Parser where

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Reader
import Parser.XML.Main
import Data.FM.Types
import Data.SPL
import Data.Tree
import Data.Either.Combinators

type Result a = (String, a)


type HephParser u a = ParsecT String u IO a


runParserM :: String -> HephParser u FeatureModel
runParserM path = do
  result <- lift $ parseFromFile parseFeatureIDE path
  if (isRight result) then
    return (fromRight' result)
  else
    return $ FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) []


parseCommand :: HephParser u (Result FeatureModel)
parseCommand = do
  result <- parseLoad <|> parseBuild
  return ("newstate", result)


parseLoad :: HephParser u FeatureModel
parseLoad =
  string "load" >> many space >> many1 letter >>= \t ->
    case t of
      "fm" -> many space >> many1 letter >>=
        \path -> runParserM path >>=
          \result -> return result


parseCK :: Parsec String () (ConfigurationKnowledge a)
parseCK = undefined

parseBuild :: HephParser u FeatureModel
parseBuild = undefined


-- TRYING TO MODEL PARSERC EFFECTS BEHAVIOUR THROUGH MONADIC TYPE CLASSES
--
-- class Monad m => MonadParser a m where
--   runParserM  :: String -> m a
--
-- instance MonadParser FeatureModel IO where
  -- runParserM path = do
  --   result <- parseFromFile parseFeatureIDE path
  --   if (isRight result) then
  --     return (fromRight' result)
  --   else
  --     return $ FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) []
