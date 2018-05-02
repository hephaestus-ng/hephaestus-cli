module Class.Logger where



class Monad m => MonadLog m where
  shLog :: String -> m ()
  shGet :: m String

instance MonadLog IO where
  shLog c = putStrLn c >> putStrLn ""
  shGet   = getLine
