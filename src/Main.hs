module Main (main) where

import Data.Text (pack)
import GHC.IO.Exception (ExitCode)
import PPrint (printProgram)
import Parser (CrystalProgram, parseProgram)
import Rearranger (rearrangeSlots)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (dropExtension, splitFileName, (<.>), (</>))
import System.Process (createProcess, shell, waitForProcess)
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
          let outputDir = dir </> fileName
          unlessIO (doesDirectoryExist outputDir) (createDirectory outputDir)
          samples <- createPrograms (rearrangeSlots out) outputDir
          _ <- runFiles samples
          -- putStrLn $ "Outputs are " ++ show outputs
          pure ()
    _ ->
      putStrLn "Usage: cabal run crystal-parser -- <input-file>"

createPrograms :: [CrystalProgram] -> String -> IO [String]
createPrograms ps dir = mapM (\(i, p) -> createProgram i p) $ zip [1 :: Int ..] ps
  where
    createProgram i p = do
      let name = dir </> show i <.> "cr"
      writeFile name $ printProgram p
      pure name

data ProcessResult = ProcessResult
  { prExitCode :: ExitCode,
    prStdout :: String,
    prStderr :: String
  }
  deriving (Show, Eq)

runFiles :: [String] -> IO [ProcessResult]
runFiles fileNames = mapM runFile fileNames
  where
    runFile fileName = do
      (_, _, _, ph) <- createProcess (shell $ "crystal run " ++ fileName)
      exitCode <- waitForProcess ph
      pure $ ProcessResult {prExitCode = exitCode, prStderr = "", prStdout = ""}

unlessIO :: IO Bool -> IO () -> IO ()
unlessIO condition action = do
  b <- condition
  if b then pure () else action
