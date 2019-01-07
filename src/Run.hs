module Run where

import Control.Monad.State

import Types.Hephaestus
import Types.State

import Shell


runHephaestus :: Env -> Hephaestus a -> IO a
runHephaestus v (Hephaestus m) = evalStateT m v


run = runHephaestus initEnv shell


initEnv = Env Nothing Nothing Nothing Nothing Nothing (Just "/Users/thi4go/hephaestus/source") (Just "/Users/thi4go/hephaestus/target/tg")
-- initEnv = Env Nothing Nothing Nothing Nothing Nothing Nothing Nothing



--
