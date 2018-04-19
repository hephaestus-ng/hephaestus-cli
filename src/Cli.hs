{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    appHeph :: ReaderT HephConfig (ExceptT HephError IO) a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader HephConfig,
    MonadError HephError,
    MonadIO
  )


readCfg :: Hephaestus String
readCfg = undefined
