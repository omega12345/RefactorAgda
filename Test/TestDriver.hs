{-# LANGUAGE OverloadedStrings #-}
module Test.TestDriver (tests) where
import Distribution.TestSuite
import System.Directory
import PrettyPrinter
import Refactoring
import Parser
import Data.Text.IO as IO
import Data.Text hiding (map)
import System.Process
import System.Exit
import Data.List (isInfixOf, isSuffixOf)

tests :: IO [Test]
tests = prettyPrintAll--[ Test succeeds, Test fails ]
  where
    succeeds = TestInstance
        { run = return $ Finished $ Pass
        , name = "succeeds"
        , tags = ["Succeeds tag"]
        , options = []
        , setOption = \_ _ -> Right succeeds
        }
fails = TestInstance
        { run = return $ Finished $ Fail "Always fails!"
        , name = "fails"
        , tags = ["fails tag"]
        , options = []
        , setOption = \_ _ -> Right fails
        }

inputDirectory :: FilePath
inputDirectory = "Test/Tests/input"

outputDirectory :: FilePath
outputDirectory = "Test/Tests/output"

prettyPrintAll :: IO [Test]
prettyPrintAll = do
  files <- listDirectory inputDirectory
  return $ map makeTest $ Prelude.filter (\x -> Data.List.isSuffixOf ".agda" x) files
  where makeTest :: FilePath -> Test
        makeTest file = Test $ TestInstance
            { run = prettyPrintFile file
            , name = file
            , tags = []
            , options = []
            , setOption = \_ _ -> Right $ fails
            }

prettyPrintFile :: FilePath -> IO Progress
prettyPrintFile file = do
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  let result = prettyPrint (reindentFile $ parse fileContents) fileContents
  let outputLocation = outputDirectory ++ "/" ++ file
  IO.writeFile outputLocation result
  result@(exitCode, stdout, stderr) <-
    withCurrentDirectory outputDirectory $ readProcessWithExitCode "agda" [file] ""
  Prelude.putStrLn $ show exitCode ++ "\n" ++ "stdout: "
      ++ stdout ++ "\nstderr: " ++ stderr
  case typecheckingOracle result of
    Pass -> return $ Finished $ Pass
    Fail x -> do
          Prelude.putStrLn $ show (parse fileContents)
          return $ Finished $ Fail x
  return $ Finished $ typecheckingOracle result
  where

typecheckingOracle :: (ExitCode, String, String) -> Result
typecheckingOracle (code, stdout, stderr) =
  if code == ExitSuccess || onlyHoles stdout
  then Pass else Fail "Output code does not compile as expected."
  where onlyHoles :: String -> Bool
        onlyHoles string = Data.List.isInfixOf "Unsolved interaction metas" string
