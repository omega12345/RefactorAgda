{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
import System.Environment

import Parser
import Data.Text as T
import Data.Text.IO as IO
import ParseTree
import PrettyPrinter
import Refactoring
import System.Console.CmdArgs

data Modes = ReindentFile {file :: String}
            | ReindentFunction {file :: String
                           , func :: String}
            | Debug {file :: String}
            | Help
              deriving (Show, Data, Typeable)

reindentFile = ReindentFile {file = def &= argPos 0 &= typFile} &= help "Reindent the whole file."

reindentFunction = ReindentFunction {file = def &= argPos 0 &= typFile
                            ,func = def &= argPos 1 &= typ "STRING"}
                            &= help "Reindent the specified function definition and type, if the function exists."

debug = Debug {file = def &= argPos 0 &= typFile} &= help "Print the parse tree, for debugging"

allModes = cmdArgsMode $ modes [Main.reindentFile, Main.reindentFunction, debug, Help &= auto &= help "Triggers helpful hint"] &= summary "The Agda refactoring program v0.1" &= program "RefactorAgda"

main = do
  command <- cmdArgsRun allModes
  case command of
    Help -> Prelude.putStrLn "Try running this command with --help."
    _ -> do
     let filename = file command
     fileContents <- IO.readFile filename
     case command of

      Debug _ -> Prelude.putStrLn $ show $ parse fileContents
      _ -> IO.putStrLn $
        prettyPrint (doRefactoring command (parse fileContents)) fileContents

doRefactoring :: Modes -> [ParseTree] -> [ParseTree]
doRefactoring (ReindentFile {}) = Refactoring.reindentFile
doRefactoring (ReindentFunction {func}) = Refactoring.reindentFunction (pack func)
