{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Commands where


import Data.FM.Types
import Data.Either.Combinators
import Text.Parsec
import Text.Parsec.String

import Parser.XML.Main
import Parser.CK
import Data.Tree
import Control.Monad.Reader

import Config


help :: IO ()
help = do
  putStrLn "---------------------------------------------------------"
  putStrLn "$ hephaestus-shell --------------------------------------"
  putStrLn "---------------------------------------------------------"
  putStrLn "                                                         "
  putStrLn "hephaestus-shell currently offers the following commands:"
  putStrLn "                                                         "
  putStrLn "$ load <type> <path-of-file>"
  putStrLn "  where load types are "
  putStrLn "    - feature model with file type .xml"
  putStrLn "      - $ load fm file.xml "
  putStrLn "                                                         "
  putStrLn "    - configuration knowledge with file type .ck "
  putStrLn "      - $ load ck file.ck "
  putStrLn "---------------------------------------------------------"



type Path = String

data Load a =
  Load {
    run :: Path -> a
  }

loadFM :: Load FeatureModel
loadFM =
  Load {
    run = do
      return $ FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) []
    }

loadF :: Load Feature
loadF =
  Load {
    run = do
      return $ Feature "iris" BasicFeature Mandatory
    }


load :: Load a -> Path -> a
load dictionary p = do
  run dictionary p


-- class Load a where
--   run :: Path -> a
--
-- instance Load Feature where
--   run p = Feature "n" BasicFeature Optional
--
--
-- load :: (Load Feature) => Path -> Feature
-- load p = run p
