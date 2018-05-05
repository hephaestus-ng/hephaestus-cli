module Action.Help where

import Control.Monad.IO.Class


help :: (MonadIO m) => m ()
help = do
  liftIO $ putStrLn "---------------------------------------------------------"
  liftIO $ putStrLn "$ hephaestus-shell --------------------------------------"
  liftIO $ putStrLn "---------------------------------------------------------"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "hephaestus-shell currently offers the following commands:"
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ load < fm | ck | asset >"
  liftIO $ putStrLn "  - load a type to Environment "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "$ show env                                                   "
  liftIO $ putStrLn "  - used to inspect Hephaestus Environment with loaded types "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "---------------------------------------------------------"


welcome :: (MonadIO m) => m ()
welcome = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""
  liftIO $ putStrLn "---------------------------------------"
  liftIO $ putStrLn "---------- Hephaestus - Shell ---------"
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""
  liftIO $ putStrLn "-- type 'help' to see available commands"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "-- loading your SPL:"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "-- 1- load fm <fm-path>"
  liftIO $ putStrLn "-- 2- load ck <ck-path>"
  liftIO $ putStrLn "-- 3- load asset ??"
  liftIO $ putStrLn ""