{-# LANGUAGE FlexibleInstances #-}

module Shell where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

-- import Config



data Command a = Load a
               | Build a

data Shell a =
    GetCmd(String -> Shell a)
  | PrintCmd String (Shell a)
  | Done a

cmd = GetCmd (\x  -> PrintCmd x (PrintCmd x (Done "end computation chain")))

interpret :: Shell a -> IO a
interpret (Done a) = pure a

interpret (GetCmd next) = do
  str <- getLine
  interpret (next str)

interpret (PrintCmd str next) = do
  putStrLn str
  interpret next


type Cmd = String
-- WITH TYPECLASS DEFINITION

class Monad m => MonadShell m where
  getCommand :: m Cmd
  runCommand :: Cmd -> m ()
  done       :: m ()

class Monad m => MonadLog m where
  logg :: String -> m ()



-- instance (HasHephConfig r) => MonadShell (ReaderT r IO) where
--   getCommand     = liftIO $ getLine
--   runCommand   s = liftIO $ putStrLn s

instance MonadShell IO where
  getCommand   = getLine
  runCommand s = putStrLn s
  done         = return ()

instance MonadLog IO where
  logg s = putStrLn s


program :: (MonadShell m, MonadLog m) => m ()
program = do
  a <- getCommand
  logg ("got a")
  b <- getCommand
  logg ("got b")
  runCommand (a ++ b)
  c <- getCommand
  runCommand c
  done
  d <- getCommand
  logg ("got d")
  runCommand d


-- main :: IO ()
-- main = myProgram
