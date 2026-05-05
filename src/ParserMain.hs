module Main (main) where

import Data.Text (pack)
import Parser (parseProgram)
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeFileName)
import PPrint (printProgram)
import Text.Megaparsec (parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      content <- pack <$> readFile input
      let fileNameWExt = takeFileName input
      let fileName = dropExtension fileNameWExt
      case parse parseProgram fileNameWExt content of
        Left err -> do
          putStrLn "Parser failed with error:"
          putStrLn $ show err
        Right out -> do
          putStrLn $ "File " <> fileName <> " parsed successfully"
          putStrLn $ printProgram out
    _ ->
      putStrLn "Usage: cabal run file-parser -- <input-file>"
