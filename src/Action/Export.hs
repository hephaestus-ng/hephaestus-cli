module Action.Export where

import Control.Lens
import Control.Monad.State

import Class.Builder


exportProduct :: (MonadBuilder m) => m ()
exportProduct = do
  src  <- gets (view src)
  trg  <- gets (view trg)
  prod <- gets (view prdct)
  exportM src trg prod
