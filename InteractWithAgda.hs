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

getTypes :: [ParseTree] -> Integer ->  [Expr] -> Text -> IO [Type]
getTypes program holeNumber exps filePath = do
  putStrLn "Starting to get types"
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
      replicateM_ 6 $ hGetLine hout
      answer3 <- System.IO.hGetLine hout -- ((last . 1) . (agda2-goals-action '(0)))
      System.IO.putStrLn $ "Test finished: "   ++ answer3
      ts <- mapM (getType hin hout file holeNumber) exps
      terminateProcess processHandle
      removeFile file
      return ts

getType :: Handle -> Handle -> String -> Integer -> Expr  -> IO Type
getType hin hout filename holeNumber expr = do
  let exprString = unpack $ printExprForAgda expr
  hPutStrLn hin $ "IOTCM \"" ++ filename ++ "\" None Indirect (Cmd_infer Simplified " ++ show holeNumber ++ " noRange \"" ++ exprString ++ "\")"
  hFlush hin
  hGetLine hout
  answer3 <- System.IO.hGetLine hout --(agda2-info-action "*Inferred Type*" "Nat" nil)
  --(agda2-info-action "*Inferred Type*" "a → a" nil)
  putStrLn answer3
  return $ findType answer3

findType :: String -> Type
findType s = case M.parse understandType "in InteractWithAgda" (pack s) of
                    Left x -> error $ errorBundlePretty x
                    Right y -> y



understandType :: Parser Type
understandType = do
    string "(agda2-info-action \"*Inferred Type*\" \""
    functionType space
