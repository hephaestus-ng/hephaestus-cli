module Action.IO where

import Control.Monad.IO.Class


help :: (MonadIO m) => m ()
help = do
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ hephaestus-shell --------------------------------------"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "hephaestus-shell currently offers the following commands:"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ load < fm | ck | pc | src | target | asset >"
  liftIO $ putStrLn "  - load a type to Environment "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ show env                                                   "
  liftIO $ putStrLn "  - used to inspect Hephaestus Environment with loaded types "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ build                                        "
  liftIO $ putStrLn "  - builds a product when environment has fm, ck and pc loaded "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ export                                        "
  liftIO $ putStrLn "  - exports a product when environment has a built product loaded"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "---------------------------------------------------------"
  liftIO $ putStrLn "                                                         "


welcome :: (MonadIO m) => m ()
welcome = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""
  liftIO $ putStrLn "---------------------------------------"
  liftIO $ putStrLn "---------- Hephaestus - Shell ---------"
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""
  liftIO $ putStrLn "   type 'help' to see available commands"
  liftIO $ putStrLn ""
  liftIO $ putStrLn " ---------------------------------------"
  liftIO $ putStrLn ""
