{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Hephaestus where

import Control.Monad.State

import Types.State

newtype Hephaestus m = Hephaestus (StateT Env IO m)
  deriving (
    Functor, Applicative, Monad,
    MonadState Env,
    MonadIO
  )
