{-# LANGUAGE OverloadedStrings #-}
module InteractWithAgda where
import ParseTree
import System.Process
import System.IO
import System.Directory
import PrettyPrinter
import Data.Text
import System.FilePath.Posix
import Text.Megaparsec.Char
import Parser
import Text.Megaparsec as M
import Control.Monad
import Data.List

output :: Text -> IO ()
output s = putStrLn $ unpack s

askAgda :: [ParseTree] -> Text -> (String -> (Handle, Handle, ProcessHandle) -> IO a) -> IO a
askAgda program filePath action = do
  putStrLn "Loading file to Agda"
  hFlush stdout
  let file = replaceFileName (unpack filePath) "RefactorAgdaTemporaryFile.agda"
  alreadyExists <- doesFileExist file
  directory <- getCurrentDirectory
  if alreadyExists then
     error $ "This would overwrite file: " ++ file
    else do
      let text = printForAgda program
      writeFile file $ unpack text
      (Just hin , Just hout, Just herr , processHandle) <- createProcess (proc "agda" ["--interaction"]){ std_out = CreatePipe , std_in = CreatePipe , std_err = CreatePipe}
      hPutStrLn hin $ "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])"
      hFlush hin
      readUntilLast hout
      System.IO.putStrLn $ "Test finished: "
      hFlush stdout
      ts <- action file (hin, hout, processHandle)
      terminateProcess processHandle
      removeFile file
      return ts

readUntilLast :: Handle -> IO ()
readUntilLast h = do
    line <- hGetLine h
    --putStrLn "in readUntilLast"
    --putStrLn line
    hFlush stdout
    if Data.List.isPrefixOf "((last . " line
      then return ()
      else readUntilLast h

readUntilInfoAction :: Handle -> IO String
readUntilInfoAction h = do
  line <- hGetLine h
  putStrLn "in readUntilLast"
  putStrLn line
  hFlush stdout
  if Data.List.isPrefixOf "(agda2-info-action" line
    then return line
    else readUntilInfoAction h


getEnvironment :: [ParseTree] -> Integer -> Text -> IO [TypeSignature]
getEnvironment program holeNumber filePath =
  askAgda program filePath $ \file (hin, hout, processHandle) -> do
      hPutStrLn hin $ "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_context Simplified " ++ show holeNumber ++ " noRange [])"

      hFlush hin
      answer3 <- readUntilInfoAction hout
      putStrLn $ "last line from getEnvironment " ++ answer3
      let env = findEnvironment answer3
      putStrLn $ "Environment found" ++ show env
      return env

getTypes :: [ParseTree] -> Integer ->  [Expr] -> Text -> IO [Type]
getTypes program holeNumber exps filePath =
  askAgda program filePath $ \file (hin, hout, processHandle) ->
      mapM (getType hin hout file holeNumber) exps

getType :: Handle -> Handle -> String -> Integer -> Expr  -> IO Type
getType hin hout filename holeNumber expr = do
  let exprString = unpack $ printExprForAgda expr
  putStrLn $ "Expression to get type from: " ++ show expr
  hPutStrLn hin $ "IOTCM \"" ++ filename ++ "\" None Indirect (Cmd_infer Simplified " ++ show holeNumber ++ " noRange \"" ++ exprString ++ "\")"
  hFlush hin
  hGetLine hout
  answer3 <- System.IO.hGetLine hout --(agda2-info-action "*Inferred Type*" "Nat" nil)
  --(agda2-info-action "*Inferred Type*" "a → a" nil)
  --putStrLn answer3

  return $ findType answer3

findType :: String -> Type
findType s = case M.parse understandType "in InteractWithAgda" (pack s) of
                    Left x -> error $ errorBundlePretty x
                    Right y -> y



understandType :: Parser Type
understandType = do
    string "(agda2-info-action \"*Inferred Type*\" \""
    functionType space

findEnvironment :: String -> [TypeSignature]
findEnvironment s = case M.parse understandEnvironment "in InteractWithAgda" (pack s) of
                    Left x -> error $ errorBundlePretty x
                    Right y -> y

--example (agda2-info-action "*Context*" "x : ℕ\ny : ℕ" nil)
understandEnvironment :: Parser [TypeSignature]
understandEnvironment = do
  string "(agda2-info-action \"*Context*\" \""
  sepBy  (typeSignature $ skipMany $ string " ") $ void $ string "\\n"
