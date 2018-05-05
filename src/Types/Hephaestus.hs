{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Hephaestus where

import Control.Monad.State

import Types.State

newtype Hephaestus a = Hephaestus (StateT Env IO a)
  deriving (
    Functor, Applicative, Monad,
    MonadState Env,
    MonadIO
  )
