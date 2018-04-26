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


help :: (MonadIO m) => m ()
help = do
  liftIO $ putStrLn "---------------------------------------------------------"
  liftIO $ putStrLn "$ hephaestus-shell --------------------------------------"
  liftIO $ putStrLn "---------------------------------------------------------"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "hephaestus-shell currently offers the following commands:"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ load <type> <path-of-file>"
  liftIO $ putStrLn "  where load types are "
  liftIO $ putStrLn "    - feature model with file type .xml"
  liftIO $ putStrLn "      - $ load fm file.xml "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "    - configuration knowledge with file type .ck "
  liftIO $ putStrLn "      - $ load ck file.ck "
  liftIO $ putStrLn "---------------------------------------------------------"



-- type Path = String
--
-- data Load a =
--   Load {
--     run :: Path -> a
--   }
--
-- loadFM :: Load FeatureModel
-- loadFM =
--   Load {
--     run = do
--       return $ FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) []
--     }
--
-- loadF :: Load Feature
-- loadF =
--   Load {
--     run = do
--       return $ Feature "iris" BasicFeature Mandatory
--     }
--
--
-- load :: Load a -> Path -> a
-- load dictionary p = do
--   run dictionary p


-- class Load a where
--   run :: Path -> a
--
-- instance Load Feature where
--   run p = Feature "n" BasicFeature Optional
--
--
-- load :: (Load Feature) => Path -> Feature
-- load p = run p
