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
  liftIO $ putStrLn "$ load <type> <path-of-file>"
  liftIO $ putStrLn "  where load types are "
  liftIO $ putStrLn "    - feature model with file type .xml"
  liftIO $ putStrLn "      - $ load fm file.xml "
  liftIO $ putStrLn "                                                         "
  liftIO $ putStrLn "    - configuration knowledge with file type .ck "
  liftIO $ putStrLn "      - $ load ck file.ck "
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
