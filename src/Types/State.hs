{-# LANGUAGE TemplateHaskell #-}

module Types.State where

import Control.Lens

import Data.FM.Types
import Data.SC.Types
import Data.SC.Asset
import Data.SPL
import Data.Tree


data FM =
  FM {
    _path     :: String,
    _parsedFM :: FeatureModel
  } deriving (Show)
makeClassy ''FM

data Env =
  Env {
    _asset :: Maybe String,
    _fm    :: Maybe FeatureModel
    -- _ck    :: Maybe (ConfigurationKnowledge ComponentModel)
  }
makeClassy ''Env


instance Show Env where
  show (Env (Just as) (Just fm)) = "Env loaded with Feature Model:" ++ show fm ++ " Env Loaded with Asset" ++ show as ++ "as"

  show (Env (Just as) Nothing) = "Environment loaded with Asset: " ++ show as

  show (Env Nothing (Just fm)) = "Environment loaded with Feature Model: " ++ show fm

  show (Env Nothing Nothing) = show "Environment is not configured"



fmcfg    = FM "fm-path" (FeatureModel (Node (Feature "iris" BasicFeature Mandatory) []) [])

envv     = Env Nothing Nothing
