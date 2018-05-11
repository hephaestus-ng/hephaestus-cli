module Class.FM where

import Control.Lens
import Control.Monad.State

import Types.Hephaestus

import Data.FM


class (Monad m) => MonadFM m where
  validateP :: FeatureModel -> ProductConfiguration -> m Bool
  pprint    :: FeatureModel -> m ()
  -- satSolve


instance MonadFM Hephaestus where

  validateP fm pc = return $ isValid fm pc

  pprint fm = liftIO $ pprintFeatureTree (view featureTree fm)


































--
