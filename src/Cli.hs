{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- extension used for deriving instances to hephaestus type

module Cli where

import Error
import Config
import Parser

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens as L


hcfg = HephConfig "asset" "ck"

-- herr = HephError 

newtype Hephaestus a =
  Hephaestus {
    appHeph :: ReaderT HephConfig (ExceptT HephError IO) a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader HephConfig,
    MonadError HephError,
    MonadIO
  )



-- load :: LoadType -> Hephaestus
