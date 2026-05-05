module Main (main) where

import System.Environment (getArgs)
import qualified Command.Parse as ParseCmd
import qualified Command.Rearrange as RearrangeCmd

main :: IO ()
main = do
  args <- getArgs
  case args of
    "parse" : cmdargs -> do
      ParseCmd.parse cmdargs
    "rearrange" : cmdargs -> do
      RearrangeCmd.rearrange cmdargs
    _ ->
      putStrLn "Usage: cabal run crystal-parser -- SUBCOMMAND DIRECTORY/FILE"
