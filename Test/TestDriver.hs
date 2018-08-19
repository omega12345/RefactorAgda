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
import Test.QuickCheck
import Data.Char
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  tests <- Main.prettyPrintAll
  moreTests <- scopeAll
  renameTests <- manualRenameTests
  pushArgTests <- manualPushArgumentTests
  defaultMain $ testGroup "all tests" [tests, moreTests, renameTests, pushArgTests]

inputDirectory :: FilePath
inputDirectory = "Test/Tests/input"

outputDirectory :: FilePath
outputDirectory = "Test/Tests/output"

prettyPrintAll :: IO TestTree
prettyPrintAll = runSameCompilationTestOnAllFiles prettyPrintFile "pretty-printing tests"

prettyPrintFile :: FilePath -> TestTree
prettyPrintFile file = testCaseSteps file $ \step -> do
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  let result1 =  rewriteAll fileContents
  let result2 = rewriteAll result1
  assertBool "prettyprint . parse . prettyprint . parse x != prettyprint . parse x" $ result1 == result2
  --step $ show $ parse fileContents
  --step $ show exitCode ++ "\n" ++ "stdout: "
    --  ++ stdout ++ "\nstderr: " ++ stderr
  typecheckingOracle result2 file
  where rewriteAll :: Text -> Text
        rewriteAll s = PrettyPrinter.prettyPrintAll (doNothing $ parse s file) (parse s file) s

scopeAll :: IO TestTree
scopeAll = runSameCompilationTestOnAllFiles scopeFile "Scoping tests"

scopeFile :: FilePath -> TestTree
scopeFile file = testCaseSteps file $ \step -> do
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  case scopeOnly $ parse fileContents file of
    Left e -> assertFailure (unpack e)
    Right x -> do
         let result = PrettyPrinter.prettyPrintAll x (parse fileContents file) fileContents
         typecheckingOracle result file

typecheckingOracle :: Text -> String -> IO ()
typecheckingOracle whatToWrite file = do
  let outputLocation = outputDirectory ++ "/" ++ file
  IO.writeFile outputLocation whatToWrite
  result@(exitCode, x, y) <- withCurrentDirectory outputDirectory $
      readProcessWithExitCode "agda" [file, "--allow-unsolved-metas"] ""
  if exitCode == ExitSuccess
  then return ()
  else assertFailure $ "Output code does not compile as expected." ++ show result

runSameCompilationTestOnAllFiles
  :: (FilePath -> TestTree) -> String -> IO TestTree
runSameCompilationTestOnAllFiles testToRun testGroupName = do
  files <- listDirectory inputDirectory
  return $ testGroup testGroupName $ map testToRun $ Prelude.filter (\x -> Data.List.isSuffixOf ".agda" x) files

manualRenameTestData :: [(FilePath, Integer, Text, Text)]
manualRenameTestData = [("Bughunting2.agda", 56, "Nat", "RenamedNat")
                       , ("SameName.agda", 61, "Stuff", "NotStuff")
                       , ("SameName.agda", 106, "Stuff", "AlsoNotStuff")
                       , ("Test.agda", 507, "plus", "thisUsedToBePlus")
                       , ("Test.agda", 192, "plus", "oncePlus")
                       , ("Test2.agda", 131, "a", "b")
                       ]

manualFailingRenameTestData :: [(FilePath, Integer, Text, Text)]
manualFailingRenameTestData = [ ("Test.agda", 1166, "ys", "empty")
                              , ("Test.agda", 466, "m", "n")
                              ]

manualRenameTests :: IO TestTree
manualRenameTests = do
  return $ testGroup "Manual rename tests" $ map manualRenameTest manualRenameTestData ++ map manualFailingRenameTest manualFailingRenameTestData

manualFailingRenameTest :: (FilePath, Integer, Text, Text) -> TestTree
manualFailingRenameTest a@(file, point, oldName, newName) =
 testCaseSteps file $ \step -> do
  step $ "starting on test case" ++ show (a)
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  let tree = parse fileContents file
  case checkCommand tree point of
    Left x -> assertFailure (unpack x)
    Right x -> do
                assertEqual "Intended indentifier and actual" oldName x
                let newContents = rename tree point newName
                case newContents of
                  Left x -> return ()
                  Right y -> assertFailure "This test is not supposed to pass."

manualRenameTest :: (FilePath, Integer, Text, Text) -> TestTree
manualRenameTest a@(file, point, oldName, newName) =
 testCaseSteps file $ \step -> do
  step $ "starting on test case" ++ show (a)
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  let tree = parse fileContents file
  case checkCommand tree point of
    Left x -> assertFailure (unpack x)
    Right x -> do
                assertEqual "Intended indentifier and actual" oldName x
                let newContents = rename tree point newName
                case newContents of
                  Left x -> assertFailure (unpack x)
                  Right y -> typecheckingOracle (prettyPrint y tree fileContents) file

manualPushArgumentTestData :: [(FilePath, Integer)]
manualPushArgumentTestData = [ ("MoveArg.agda" , 78)
                             ,("MoveArg.agda" , 249)
                             ]

manualFailingPushTestData :: [(FilePath, Integer)]
manualFailingPushTestData = [ ("MoveArg.agda" , 235)
                        --     ,("MoveArg.agda" , 249)
                             ]

manualPushArgumentTests :: IO TestTree
manualPushArgumentTests = do
  return $ testGroup "Manual argument pushing tests" $ map manualPushArgumentTest manualPushArgumentTestData ++ map manualFailingPushTest manualFailingPushTestData

manualPushArgumentTest :: (FilePath, Integer) -> TestTree
manualPushArgumentTest (file , point) =
  testCaseSteps file $ \step -> do
    fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
    let tree = parse fileContents file
    let newContents = pushArgument tree point
    case newContents of
      Left x -> assertFailure (unpack x)
      Right y -> typecheckingOracle (prettyPrint y tree fileContents) file

manualFailingPushTest :: (FilePath, Integer) -> TestTree
manualFailingPushTest (file, point) = do
  testCaseSteps file $ \step -> do
    fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
    let tree = parse fileContents file
    let newContents = pushArgument tree point
    case newContents of
      Left x -> return ()
      Right y ->  do
                   typecheckingOracle (prettyPrint y tree fileContents) file
                   assertFailure "Failing push test passed."
  -- how to combine quickcheck with io
  --return $ testGroup "Manual rename tests with auto-generated new names" [testProperty "Rename does not crash" $ (\x -> ioProperty $ testScoping fileContents x)]

renameDoesNotCrash :: Text -> NamePart -> IO ()
renameDoesNotCrash code newname = undefined


newtype NamePart = NamePart {convertToText :: Text} deriving Show

instance Arbitrary NamePart where
  arbitrary = generateNamePart

generateNamePart :: Gen NamePart
generateNamePart = fmap (NamePart . pack) $ listOf1 $ suchThat arbitraryUnicodeChar $ (\x -> isPrint x && not (elem x (" @.(){};_"::String)))
