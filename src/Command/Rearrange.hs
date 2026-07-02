module Command.Rearrange (rearrange) where

import Control.Monad (unless, zipWithM)
import Data.Text (pack)
import GHC.IO.Exception (ExitCode)
import PPrint (printProgram)
import Parser (CrystalProgram, parseProgram)
import Rearranger (rearrangeSlots)
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, takeFileName, (<.>), (</>))
import System.Process (readProcessWithExitCode)
import qualified Text.Megaparsec as Megaparsec

rearrange :: [String] -> IO ()
rearrange args = do
  case args of
    [inputsDir] -> do
      case takeExtension inputsDir of
        "" -> do
          files <- filter (\fn -> takeExtension fn == ".cr") <$> listDirectory inputsDir
          mapM_ (oneFile inputsDir) files
        ".cr" ->
          oneFile "" inputsDir
        _ -> do
          putStrLn "Can only take a directory or a file with \".cr\" extension as an input"
          putStrLn "Usage: cabal run crystal-tool -- rearrange <DIRECTORY/FILE>"
    _ ->
      putStrLn "Usage: cabal run crystal-tool -- rearrange <DIRECTORY/FILE>"

oneFile :: FilePath -> FilePath -> IO ()
oneFile dir fileName = do
  -- TODO: bad naming: fileName, name, filePath, dir ¿¿??
  content <- pack <$> readFile (dir </> fileName)
  let name = dropExtension fileName
  case Megaparsec.parse parseProgram fileName content of
    Left err -> do
      -- TODO: improve error messages
      putStrLn $ "Failed parsing file: " <> fileName
      putStrLn $ "With error " <> show err
    Right out -> do
      let outputDir = dir </> name
      unlessIO (doesDirectoryExist outputDir) (createDirectory outputDir)
      samples <- createPrograms (rearrangeSlots out) outputDir
      outputs <- mapM (runExperiment outputDir samples) crystalOpts
      saveResults outputDir outputs (outputDir </> "result.md")
      pure ()

createPrograms :: [CrystalProgram] -> String -> IO [String]
createPrograms ps dir = zipWithM createProgram [1 :: Int ..] ps
  where
    createProgram i p = do
      let name = dir </> show i <.> "cr"
      writeFile name $ printProgram p
      pure name

data Experiment = Experiment
  { expName :: String,
    expResults :: [ProcessResult],
    expCrystalOpts :: [String]
  }
  deriving (Show, Eq)

data ProcessResult = ProcessResult
  { prFileName :: String,
    prExitCode :: ExitCode,
    prStdout :: String,
    prStderr :: String
  }
  deriving (Show, Eq)

crystalOpts :: [[String]]
crystalOpts =
  [ [],
    ["-Dpreview_overload_order"]
  ]

runExperiment :: String -> [String] -> [String] -> IO Experiment
runExperiment name files opts = do
  results <- runFiles opts files
  pure $
    Experiment
      { expName = name,
        expCrystalOpts = opts,
        expResults = results
      }

runFiles :: [String] -> [String] -> IO [ProcessResult]
runFiles opts files = mapM runFile files
  where
    runFile file = do
      (exitCode, out, err) <- readProcessWithExitCode "crystal" (["run", file] ++ opts) ""
      pure $
        ProcessResult
          { prFileName = takeFileName file,
            prExitCode = exitCode,
            prStderr = err,
            prStdout = out
          }

-- TODO: improve format for saving results
saveResults :: String -> [Experiment] -> String -> IO ()
saveResults sampleName exps fn = writeFile fn (unlines content)
  where
    content = ("# Sample " ++ sampleName) : concatMap showExp exps
    showExp expr =
      let prs = expResults expr in
        ["## Flags " ++ (show $ expCrystalOpts expr)] ++
        exitCodes prs ++ stdOuts prs ++ stdErrs prs ++ conclusion prs
    listWith prs f = map (\pr -> (dropExtension $ prFileName pr) ++ ". " ++ (show $ f pr)) prs
    exitCodes prs = "### Exit codes " : listWith prs prExitCode
    stdOuts prs = "### Stdouts " : listWith prs prStdout
    stdErrs prs = "### Stderrs " : listWith prs prStderr
    conclusion prs = "### Result" : [show $ allEqual prs]

    allEqual [] = True
    allEqual (x : xs) =
      all
        ( \y ->
            prExitCode x == prExitCode y
              && prStderr x == prStderr y
              && prStdout x == prStdout y
        )
        xs

unlessIO :: IO Bool -> IO () -> IO ()
unlessIO condition action = do
  b <- condition
  unless b action
