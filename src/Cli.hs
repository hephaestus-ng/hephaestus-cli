{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- extension used for deriving instances to hephaestus type

module Cli where

import Error
import Config
import Parser
import Shell


import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens as L


hcfg     = HephConfig "asset-path" "ck-path"

splerr   = FeatureModelErr "invalid feature model"
asseterr = ParserTErr "invalid path"

herr     = HephSPLError splerr


newtype Hephaestus a =
  Hephaestus {
    appHeph :: ReaderT HephConfig IO a
    -- appHeph :: ReaderT HephConfig (ExceptT HephError IO) a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader HephConfig,
    MonadIO
  )

data Env = Env
  { envLog     :: String -> IO ()
  }

env = Env { envLog = \x -> putStrLn x }

modifyCfg :: (MonadReader Env m, MonadIO m) => String -> m ()
modifyCfg f = do
  cfg <- ask
  liftIO $ envLog cfg f

main :: IO ()
main = do
  runReader (modifyCfg "la") env
