module Command.Parse (parse) where

import Data.Text (pack)
import Parser (parseProgram)
import System.FilePath (dropExtension, takeFileName)
import PPrint (printProgram)
import qualified Text.Megaparsec as Megaparsec

parse :: [String] -> IO ()
parse args =
  case args of
    [input] -> do
      content <- pack <$> readFile input
      let fileNameWExt = takeFileName input
      let fileName = dropExtension fileNameWExt
      case Megaparsec.parse parseProgram fileNameWExt content of
        Left err -> do
          putStrLn "Parser failed with error:"
          putStrLn $ show err
        Right out -> do
          putStrLn $ "File " <> fileName <> " parsed successfully"
          putStrLn $ printProgram out
    _ ->
      putStrLn "Usage: cabal run crystal-tool -- parse <input-file>"
