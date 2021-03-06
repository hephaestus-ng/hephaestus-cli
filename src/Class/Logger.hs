module Class.Logger where

import Types.Hephaestus


class Monad m => MonadLog m where
  shLog :: String -> m ()
  shGet :: m String

instance MonadLog Hephaestus where
  shLog c = putStrLn "" >> putStrLn c >> putStrLn ""
  shGet   = getLine
