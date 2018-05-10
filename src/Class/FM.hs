module Class.FM where

import Types.Hephaestus

import Control.Monad.State

import Data.FM.Types
import Data.FM.ProductConfiguration
import Data.FM.Utils




class (Monad m) => MonadFM m where
  validateP :: FeatureModel -> ProductConfiguration -> m Bool
  pprint    :: FeatureTree -> m ()
  -- satSolve



instance MonadFM Hephaestus where

  validateP fm pc = return $ isValid fm pc

  pprint ft = liftIO $ pprintFeatureTree ft


































--
