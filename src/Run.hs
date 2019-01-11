module Run where

import Control.Monad.State

import Types.Hephaestus
import Types.State

import Shell


runHephaestus :: Env -> Hephaestus a -> IO a
runHephaestus v (Hephaestus m) = evalStateT m v


run = runHephaestus initEnv shell


initEnv = Env Nothing Nothing Nothing Nothing Nothing Nothing (Just "/c/Users/Pichau/source/") (Just "/c/Users/Pichau/target/")
-- initEnv = Env Nothing Nothing Nothing Nothing Nothing Nothing Nothing



--
