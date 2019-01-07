{-# LANGUAGE OverloadedStrings #-}

import           Data.Char
import           Data.List                (isInfixOf, isSuffixOf)
import           Data.Text                hiding (map)
import           Data.Text.IO             as IO
import           MAlonzo.Code.Refactoring
import           Parser
import           ParseTree
import           PrettyPrinter
import           System.Directory
import           System.Exit
import           System.Process
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
--import InteractWithAgda

main :: IO ()
main = do
  tests <- Main.prettyPrintAll
  moreTests <- runSameCompilationTestOnAllFiles scopeFile "Scoping tests"
  passingRenameTests <- generateTests manualRenameTest
                                      manualRenameTestData
                                      "Passing manual rename tests"
  failingManualRenameTests <- generateTests manualFailingRenameTest
                                            manualFailingRenameTestData
                                            "Failing manual rename tests"
  passingPushArgTests <- generateTests manualPushArgumentTest
                                       manualPushArgumentTestData
                                       "Passing push tests"
  failingPushArgTests <- generateTests manualFailingPushTest
                                       manualFailingPushTestData
                                       "Failing push tests"
  functionExtraction <- generateTests functionExtractionTest
                                      functionExtractionTestData
                                      "function extraction tests"
  conversion <- generateTests manualExplicitImplicitConversionTest
                              manualExplicitImplicitConversionTestData
                              "conversion between explicit and implicit tests"
  defaultMain $ testGroup "all tests" [tests, moreTests, passingRenameTests,
    failingManualRenameTests, passingPushArgTests, failingPushArgTests, functionExtraction, conversion]
  --defaultMain $ testGetTypes

inputDirectory :: FilePath
inputDirectory = "RefactorAgdaEngine/Test/Tests/input"

outputDirectory :: FilePath
outputDirectory = "RefactorAgdaEngine/Test/Tests/output"

importTestFileDirectory :: FilePath
importTestFileDirectory = outputDirectory ++ "/" ++ "ImportTests"

generateTests :: (a -> TestTree) -> [a] -> String -> IO TestTree
generateTests f xs groupName = return $ testGroup groupName $ map f xs

prettyPrintAll :: IO TestTree
prettyPrintAll = runSameCompilationTestOnAllFiles prettyPrintFile "pretty-printing tests"

prettyPrintFile :: FilePath -> TestTree
prettyPrintFile file = testCaseSteps file $ \step -> do
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  let tree1 = parse fileContents file
  let result1 =  PrettyPrinter.prettyPrintAll tree1 tree1 fileContents
  let tree2 = parse result1 file
  let result2 = PrettyPrinter.prettyPrintAll tree2 tree2 result1
  let (Just (common, rest1, rest2)) = commonPrefixes result1 result2
  assertBool ("prettyprint . parse . prettyprint . parse x != prettyprint . parse x. Rest 1 = \n" ++ unpack rest1 ++ "\nRest 2 = " ++ unpack rest2) $ result1 == result2
  --step $ show $ parse fileContents
  --step $ show exitCode ++ "\n" ++ "stdout: "
    --  ++ stdout ++ "\nstderr: " ++ stderr
  typecheckingOracle result2 file

scopeFile :: FilePath -> TestTree
scopeFile file = testCaseSteps file $ \step -> do
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  scoped <- scopeOnly $ parse fileContents file
  case scoped of
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

importCompileOracle :: Text -> String -> IO ()
importCompileOracle whatToWrite file = do
  --Prelude.putStrLn $  "Stuff we got to print: " ++ unpack whatToWrite
  let outputLocation = outputDirectory ++ "/" ++ file
  IO.writeFile outputLocation whatToWrite
  result@(exitCode, x, y) <- withCurrentDirectory outputDirectory $
      readProcessWithExitCode "agda" ["ImportTests/" ++ file] ""
  if exitCode == ExitSuccess
  then return ()
  else assertFailure $ "Output code does not compile as expected." ++ show result

runSameCompilationTestOnAllFiles
  :: (FilePath -> TestTree) -> String -> IO TestTree
runSameCompilationTestOnAllFiles testToRun testGroupName = do
  files <- listDirectory inputDirectory
  return $ testGroup testGroupName $ map testToRun $ Prelude.filter (\x -> Data.List.isSuffixOf ".agda" x) files

pointBetweenStrings :: Text -> Text -> Text -> IO Integer
pointBetweenStrings code beforePoint afterPoint = do
    let (before , rest) = breakOn (append beforePoint afterPoint) code
    let result = toInteger $ Data.Text.length before + Data.Text.length beforePoint
    if Data.Text.null before
      then do Prelude.putStrLn "Are you sure you meant the first line of the file?"
              return result
      else return result

pointsBetweenStrings :: Text -> Text -> Text -> Text -> IO (Integer, Integer)
pointsBetweenStrings code before inside after = do
    let (b , rest) = breakOn (append before inside `append` after) code
    let point1 = toInteger $ Data.Text.length b + Data.Text.length before
    let point2 = toInteger $ Data.Text.length b + Data.Text.length before
                  + Data.Text.length inside
    if Data.Text.null b
      then do Prelude.putStrLn "Are you sure you meant the first line of the file?"
              return (point1, point2)
      else return (point1, point2)

manualRenameTestData :: [(FilePath, Text, Text, Text, Text)]
manualRenameTestData =
  [ ("Bughunting2.agda", "data Nat : Set where zero : Na", "t", "Nat", "RenamedNat")
  , ("SameName.agda", "  mkStuff : Stu", "ff", "Stuff", "NotStuff")
  , ("SameName.agda", "stuffFunc : Stuff -> (Stuff : Set) -> St", "uff", "Stuff", "AlsoNotStuff")
  , ("Test.agda", "{-# BUILTIN NATPLUS pl", "us #-}", "plus", "thisUsedToBePlus")
  , ("Test.agda", "pl", "us {- preserved comment  {- which may be nested -} -} :", "plus", "oncePlus")
  , ("Test2.agda", "f a", " = 3", "a", "b")
  ]

manualFailingRenameTestData :: [(FilePath, Text, Text, Text, Text)]
manualFailingRenameTestData =
    [ ("Test.agda", "append empty y", "s = ys", "ys", "empty")
    , ("Test.agda", "plus (suc n) m", " = suc (plus n m)", "m", "n")
    ]



manualFailingRenameTest :: (FilePath, Text, Text, Text, Text) -> TestTree
manualFailingRenameTest a@(file, beforePoint, afterPoint, oldName, newName) =
 localOption (mkTimeout 1000000) $ testCaseSteps file $ \step -> do
  --step $ "starting on test case" ++ show (a)
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  point <- pointBetweenStrings fileContents beforePoint afterPoint
  let tree = parse fileContents file
  correctCommand <- checkCommand tree point
  case correctCommand of
    Left x -> assertFailure (unpack x)
    Right x -> do
                assertEqual "Intended indentifier and actual" oldName x
                newContents <- rename tree point newName
                case newContents of
                  Left x -> return ()
                  Right y -> assertFailure "This test is not supposed to pass."

manualRenameTest :: (FilePath, Text, Text, Text, Text) -> TestTree
manualRenameTest a@(file, beforePoint, afterPoint, oldName, newName) =
 localOption (mkTimeout 1000000) $ testCaseSteps file $ \step -> do
  --step $ "starting on test case" ++ show (a)
  fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
  newPoint <- pointBetweenStrings fileContents beforePoint afterPoint
  let tree = parse fileContents file
  correctCommand <- checkCommand tree newPoint
  case correctCommand of
    Left x -> assertFailure (unpack x)
    Right x -> do
                assertEqual "Intended indentifier and actual" oldName x
                newContents <- rename tree newPoint newName
                case newContents of
                  Left x -> assertFailure (unpack x)
                  Right y -> typecheckingOracle (prettyPrint y tree fileContents) file

manualOnePointTest :: ([ParseTree] -> Integer -> IO (Either Text [ParseTree]))
    -> (FilePath, Text, Text) -> TestTree
manualOnePointTest f (file , before, after) =
  localOption (mkTimeout 1000000) $ testCaseSteps file $ \step -> do
    fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
    point <- pointBetweenStrings fileContents before after
    let tree = parse fileContents file
    newContents <- f tree point
    case newContents of
      Left x  -> assertFailure (unpack x)
      Right y -> typecheckingOracle (prettyPrint y tree fileContents) file

manualPushArgumentTestData :: [(FilePath, Text, Text)]
manualPushArgumentTestData =
  [ ("MoveArg.agda" , "stuff : ( num", "ber : Nat) -> Bool -> Bool")
  ,("MoveArg.agda" , "dep : (A : Set) -> (B", " : A) -> Bool -> Bool")
  ,("MoveArg.agda", "nonDep : (A", " : Set) -> (B : Set) -> B -> B")
  ]

manualFailingPushTestData :: [(FilePath, Text, Text)]
manualFailingPushTestData =
  [ ("MoveArg.agda" , "dep : (", "A : Set) -> (B : A) -> Bool -> Bool")
  ,("MoveArg.agda" ,"unnamedDep : (A", " : Set) -> A -> Bool -> Bool")
  ,("MoveArg.agda", "sameName : (", "A : Set) -> {A : Set} -> A -> A")
  ]

manualPushArgumentTest :: (FilePath, Text, Text) -> TestTree
manualPushArgumentTest = manualOnePointTest pushArgument

manualFailingPushTest :: (FilePath, Text, Text) -> TestTree
manualFailingPushTest (file, before, after) = do
  localOption (mkTimeout 1000000) $ testCaseSteps file $ \step -> do
    fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
    point <- pointBetweenStrings fileContents before after
    let tree = parse fileContents file
    newContents <- pushArgument tree point
    case newContents of
      Left x -> return ()
      Right y ->  do
                   --typecheckingOracle (prettyPrint y tree fileContents) file
                   assertFailure "Failing push test passed."

manualExplicitImplicitConversionTestData :: [(FilePath, Text, Text)]
manualExplicitImplicitConversionTestData =
    [("Conversion.agda", "nonDependent : Nat -> Na", "t -> Nat")
    ,("Conversion.agda", "dependent : {A : Set} -> A", " -> A")
    ]

manualExplicitImplicitConversionTest :: (FilePath, Text, Text) -> TestTree
manualExplicitImplicitConversionTest = manualOnePointTest toggleExplicitness

functionExtractionTestData :: [(FilePath, Text, Text, Text)]
functionExtractionTestData =
    [ ("ExtractFunction.agda" , "function1 x y = plus ", "x y", "")
    ,("ExtractFunction.agda" , "function2 x y = pickT", "heFirst x y", "")
    , ("ExtractDependent.agda" , "apply A B f a = f ", "a", "")
    , ("ExtractDependent.agda" , "applySameName C A B g f a = ", "f a", "")
    , ("ExtractDependent.agda" , "applyImp f a = f ", "a", "")
    , ("ExtractDependent.agda" , "applyImpSameName A B h = ", "B h", "")
    , ("ExtractCaseSplit.agda", "func (just x) = n", "ot x", "")
    , ("ExtractCaseSplit.agda", "func (just x) = not ", "x", "")
    , ("ExtractCaseSplit.agda", "func2 (x ∷ x₁) = not (f", "unc2 x", "₁)")
    , ("ExtractFunction.agda", "function2 x y = pi", "ckTheFirst x", " y")
    ]

functionExtractionTest :: (FilePath, Text, Text, Text) -> TestTree
functionExtractionTest (file , before, inside, after) =
  localOption (mkTimeout 15000000) $ testCaseSteps file $ \step -> do
    fileContents <- IO.readFile $ inputDirectory ++ "/" ++ file
    (startPoint, endPoint) <- pointsBetweenStrings fileContents before inside after
    let tree = parse fileContents file
    newContents <- extractFunction tree startPoint endPoint $ pack $ inputDirectory ++ "/" ++ file
    case newContents of
      Left x -> assertFailure (unpack x)
      Right y -> do
            --step $ "Parse tree: " ++ show newContents
            importCompileOracle (PrettyPrinter.prettyPrintAll y tree fileContents) file

testGetTypes :: TestTree
testGetTypes =
  localOption (mkTimeout 1000000) $
    testCaseSteps "I'm a file name" $ \step -> do
      Prelude.putStrLn "In test get types"
      fileContents <- IO.readFile $ inputDirectory ++ "/" ++ "Bughunting.agda"
      let tree = parse fileContents "I'm a name"
      a <- MAlonzo.Code.Refactoring.extractFunction tree 5 5 $ pack $ inputDirectory ++ "/" ++ "Bughunting.agda"
      --Prelude.putStrLn $ show a
      return ()

  -- how to combine quickcheck with io
  --return $ testGroup "Manual rename tests with auto-generated new names" [testProperty "Rename does not crash" $ (\x -> ioProperty $ testScoping fileContents x)]

newtype NamePart = NamePart {convertToText :: Text} deriving Show

instance Arbitrary NamePart where
  arbitrary = generateNamePart

generateNamePart :: Gen NamePart
generateNamePart = fmap (NamePart . pack) $ listOf1 $ suchThat arbitraryUnicodeChar $ (\x -> isPrint x && not (elem x (" @.(){};_"::String)))
