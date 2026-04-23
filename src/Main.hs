module Main (main) where

import Data.Text (pack)
import PPrint (printProgram)
import Parser (parseProgram)
import Rearranger (rearrangeSlots)
import System.Directory (createDirectory)
import System.Environment (getArgs)
import System.FilePath (dropExtension, splitFileName, (<.>), (</>))
import Text.Megaparsec (parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      content <- pack <$> readFile input
      let (dir, fileNameWExt) = splitFileName input
      let fileName = dropExtension fileNameWExt
      case parse parseProgram fileNameWExt content of
        Left _ ->
          putStrLn "Parser failed"
        Right out -> do
          let indexed = zip [1 ..] (rearrangeSlots out)
          let outputDir = dir </> fileName
          createDirectory outputDir
          mapM_
            (\(i, progr) -> writeFile (outputDir </> show i <.> "cr") (printProgram progr))
            indexed
    _ ->
      putStrLn "Usage: cabal run crystal-parser -- <input-file>"
