module Main (main) where

import Data.Text (pack)
import GHC.IO.Exception (ExitCode)
import PPrint (printProgram)
import Parser (CrystalProgram, parseProgram)
import Rearranger (rearrangeSlots)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (dropExtension, splitFileName, takeFileName, (<.>), (</>))
import System.Process (readProcessWithExitCode)
import Text.Megaparsec (parse)
import Control.Monad (zipWithM)

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
          let outputDir = dir </> fileName
          unlessIO (doesDirectoryExist outputDir) (createDirectory outputDir)
          samples <- createPrograms (rearrangeSlots out) outputDir
          outputs <- runFiles samples
          saveResults outputs (outputDir </> "result.md")
          pure ()
    _ ->
      putStrLn "Usage: cabal run crystal-parser -- <input-file>"

createPrograms :: [CrystalProgram] -> String -> IO [String]
createPrograms ps dir = zipWithM createProgram [1 :: Int ..] ps
  where
    createProgram i p = do
      let name = dir </> show i <.> "cr"
      writeFile name $ printProgram p
      pure name

data ProcessResult = ProcessResult
  { prFileName :: String,
    prExitCode :: ExitCode,
    prStdout :: String,
    prStderr :: String
  }
  deriving (Show, Eq)

runFiles :: [String] -> IO [ProcessResult]
runFiles = mapM runFile
  where
    runFile filePath = do
      (exitCode, out, err) <- readProcessWithExitCode "crystal" ["run", filePath] ""
      pure $ ProcessResult {
        prFileName = takeFileName filePath,
        prExitCode = exitCode,
        prStderr = err,
        prStdout = out
      }

saveResults :: [ProcessResult] -> String -> IO ()
saveResults rs fn = writeFile fn (unlines content)
  where
    content =
      [ "| Sample name | Exit code | Stdout | Stderr |",
        "| ----------- | --------- | ------ | ------ |"
      ]
      ++ map (\s -> foldr (\f rec -> "| " ++ show (f s) ++ rec) " |" [prFileName, show . prExitCode, prStdout, prStderr]) rs
      ++ [ "\nAll samples produce same result when executing? " ++ show (allEqual rs) ]

    allEqual [] = True
    allEqual (x:xs) = all
      (\y -> prExitCode x == prExitCode y
          && prStderr x == prStderr y
          && prStdout x == prStdout y) xs

unlessIO :: IO Bool -> IO () -> IO ()
unlessIO condition action = do
  b <- condition
  if b then pure () else action
