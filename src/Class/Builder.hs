module Class.Builder where

import Data.Maybe

import Control.Monad.State
import Control.Lens

import Data.FM
import Data.SPL
import Data.SC

import Types.Hephaestus
import Types.State


class Monad m => MonadBuilder m where
  buildM :: FeatureModel ->
            ConfigurationKnowledge ComponentModel ->
            ProductConfiguration ->
            m (Product ComponentModel)


instance MonadBuilder Hephaestus where
  buildM fm ck pc = do
    let spl = SPL fm ck
    return $ build spl pc
