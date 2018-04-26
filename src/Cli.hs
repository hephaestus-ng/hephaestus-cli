{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- extension used for deriving instances to hephaestus type

module Cli where

import Error
import Config
import Parser
import Types
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



class Monad m => MonadLog m where
  shLog :: String -> m ()

instance MonadLog IO where
  shLog c = putStrLn c


shell :: (MonadIO m, MonadParser FeatureModel m) => m ()
shell = liftIO $ welcome >> shellLoop
  where
    shellLoop = do
      cmd <- liftIO $ getLine
      -- handleCmd cmd
      fm <- loadFM "fm.ide"
      liftIO $ print fm
      shellLoop



-- lloadFM ::

load :: (MonadParser FeatureModel m,
         MonadParser (ConfigurationKnowledge TestAsset) m,
         MonadLog m, MonadIO m) => String -> m ()
load s = do
  shLog "loading asset"
  res <- loadFM "fm.ide"
  liftIO $ print res
  shLog "agora o ck"
  res2 <- loadCK
  shLog "agora a shell"
  shell


-- handleCmd :: String -> IO ()
-- handleCmd cmd = do
--   shLog ("running command "++cmd)
--   case cmd of
--     "load fm" -> loadFM
--     "load ck" -> loadCK

  -- liftIO $ putStrLn result


-- handleLoadFM :: IO ()
-- handleLoadFM = do
--   return $ loadFM "fm.ide"

testP :: IO ()
testP = do
  fm <- loadFM "fm.ide"
  liftIO $ print fm

readConfig :: (MonadReader env m, HasFM env) => m FM
readConfig = do
  cfg <- ask
  return (view fmConfig cfg)


welcome :: (MonadIO m, MonadLog m) => m ()
welcome = do
  shLog ("---------------------------------------" )
  shLog ("---------- Hephaestus - Shell ---------" )
  shLog ("--" )
  shLog ("--" )
  shLog ("-- type 'help' to see available commands" )
  shLog ("--" )
  shLog ("-- loading your SPL:" )
  shLog ("--" )
  shLog ("-- 1- load fm <fm-path>" )
  shLog ("-- 2- load ck <ck-path>" )
  shLog ("-- 3- load asset ??" )
  shLog ("--" )




newtype Hephaestus a =
  Hephaestus {
    appHeph :: ReaderT Env IO a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader Env,
    MonadIO
  )

main :: Hephaestus ()
main = undefined
