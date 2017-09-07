{-# OPTIONS_GHC -O2 #-}
-- | Module to run the various engines
module Proba.Run (
  main
) where

import Proba.CLI
import Control.DeepSeq
import Control.Exception
import System.Environment
import Text.Printf

-- | Main function to run the engine
main :: IO()
main = do
  args <- getArgs
  let printHelp =
        mapM_ (\ (x, y) -> printf "%-55s: %s \n" x y) help

  if "-help" `elem` args
     then printHelp
     else do
        query <- try $
            evaluate $ force $ parse args

        case query :: Either SomeException (Maybe Query) of
          Right (Just q) -> do
            putStrLn "Running query: "
            print q
            putStrLn ""
            r <- run q
            case r of
              Left err -> do
                putStrLn "There was an error during the run: "
                putStrLn err
              Right res -> print res
          _ -> do
            putStrLn "Error in the input argurments"
            putStrLn "Follow the help below: "
            putStrLn ""
            printHelp
