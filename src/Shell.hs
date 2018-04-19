{-# LANGUAGE FlexibleInstances #-}

module Shell where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Config
-- FREE MONAD EXAMPLE : CHAINING COMPUTATIONS FROM MONADIC SHELL
-- data Command = Load
--              | Build
--
-- data Shell a =
--     GetLine (String -> Shell a)
--   | PrintLine String (Shell a)
--   | Done a

-- cmd = GetLine (\x  -> PrintLine x (Done "done"))

-- interpret :: Shell a -> IO a
-- interpret (Done a) = pure a
-- interpret (GetLine next) = do
--   str <- getLine
--   interpret (next str)
-- interpret (PrintLine str next) = do
--   putStrLn str
--   interpret next


class Monad m => MonadShell m where
  getCommand :: m String
  runCommand :: String -> m ()



instance (HasHephConfig r) => MonadShell (ReaderT r IO) where
  getCommand     = liftIO $ getLine
  runCommand   s = liftIO $ putStrLn s



myProgram :: MonadShell m => m ()
myProgram = do
  a <- getCommand
  b <- getCommand
  runCommand (a ++ b)


-- main :: IO ()
-- main = myProgram
