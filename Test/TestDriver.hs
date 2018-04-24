{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import PrettyPrinter
import MAlonzo.Code.Refactoring
import Parser
import Data.Text.IO as IO
import Data.Text hiding (map)
import System.Process
import System.Exit
import Data.List (isInfixOf, isSuffixOf)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  tests <- prettyPrintAll
  defaultMain tests

inputDirectory :: FilePath
inputDirectory = "Test/Tests/input"

outputDirectory :: FilePath
outputDirectory = "Test/Tests/output"

prettyPrintAll :: IO TestTree
prettyPrintAll = do
  files <- listDirectory inputDirectory
  return $ testGroup "all tests" $ map prettyPrintFile $ Prelude.filter (\x -> Data.List.isSuffixOf ".agda" x) files

prettyPrintFile :: FilePath -> TestTree
prettyPrintFile file = testCaseSteps file $ \step -> do
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  let result = prettyPrint (reindentFile $ parse fileContents) fileContents
  let outputLocation = outputDirectory ++ "/" ++ file
  IO.writeFile outputLocation result
  result@(exitCode, stdout, stderr) <-
    withCurrentDirectory outputDirectory $ readProcessWithExitCode "agda" [file] ""
  step $ show $ parse fileContents
  step $ show exitCode ++ "\n" ++ "stdout: "
      ++ stdout ++ "\nstderr: " ++ stderr
  typecheckingOracle result

typecheckingOracle :: (ExitCode, String, String) -> IO ()
typecheckingOracle (code, stdout, stderr) =
  if code == ExitSuccess || onlyHoles stdout
  then return ()
  else assertFailure "Output code does not compile as expected."
  where onlyHoles :: String -> Bool
        onlyHoles string = Data.List.isInfixOf "Unsolved interaction metas" string
