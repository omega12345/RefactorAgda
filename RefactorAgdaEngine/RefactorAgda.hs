{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                (concat, pack, unpack)
import           Data.Text.IO             as IO
import           MAlonzo.Code.Refactoring
import           Parser
import           PrettyPrinter
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
      filename : rest ->
        do
          fileContents <- IO.readFile filename
          let tree = parse fileContents filename
          case rest of
           ["rename", offset, newName] ->
            do
              let point = read offset :: Integer
              newContents <- rename tree point $ pack newName
              case newContents of
                Left x -> error $ "start of RefactorAgda output" ++ unpack x
                Right y -> IO.putStrLn $ Data.Text.concat ["start of RefactorAgda output", prettyPrint y tree fileContents]
           ["push", offset] ->
              do
                let point = read offset :: Integer
                newContents <- pushArgument tree point
                case newContents of
                  Left x -> error $ "start of RefactorAgda output" ++ unpack x
                  Right y -> IO.putStrLn $ Data.Text.concat ["start of RefactorAgda output", prettyPrint y tree fileContents]
           ["convert", offset] ->
              do
                let point = read offset :: Integer
                newContents <- pushArgument tree point
                case newContents of
                  Left x -> error $ "start of RefactorAgda output" ++ unpack x
                  Right y -> IO.putStrLn $ Data.Text.concat ["start of RefactorAgda output", prettyPrint y tree fileContents]
           ["extractFunction", startOffset, endOffset] ->
            do
              let start = read startOffset :: Integer
              let end = read endOffset :: Integer
              newContents <- extractFunction tree start end (pack filename)
              case newContents of
                Left x -> error $ "start of RefactorAgda output" ++ unpack x
                Right y -> IO.putStrLn $ Data.Text.concat ["start of RefactorAgda output", prettyPrintAll y tree fileContents]
           ["toggleExplicitness", offset] ->
             do
              let point = read offset :: Integer
              newContents <- toggleExplicitness tree point
              case newContents of
                Left x -> error $ "start of RefactorAgda output" ++ unpack x
                Right y -> IO.putStrLn $ Data.Text.concat ["start of RefactorAgda output", prettyPrint y tree fileContents]
           _ -> error $ "start of RefactorAgda output" ++ "Wrong number of arguments" ++ show rest
      _ -> error $ "start of RefactorAgda output" ++ "Not even a file name"
