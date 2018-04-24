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
