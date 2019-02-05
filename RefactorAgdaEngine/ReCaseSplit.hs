module ReCaseSplit where
import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity
import           Data.List
import           Data.Void
import           System.IO
import           System.Process
import           System.Timeout
import           Text.Megaparsec
import           Text.Megaparsec.Char

--main :: IO ()
--main = do
    --ans <- reCaseSplit "Test/Tests/ManualTestFiles/ReCaseSplit.agda"
--    ans <- reCaseSplit "ParseTreeOperations.agda"
--    putStrLn ans

data ChangeIns = ChangeIns [String] Int Int deriving Show

reCaseSplit :: String -> IO String
reCaseSplit file = do
  putStrLn "Loading file to Agda"
  hFlush stdout
  (Just hin , Just hout, Just herr , processHandle) <- createProcess (proc "agda" ["--interaction"]){ std_out = CreatePipe , std_in = CreatePipe , std_err = CreatePipe}
  hPutStrLn hin $ "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])"
  hFlush hin
  o <- timeout 20000000 $ readUntilLast hout
  case o of
    Nothing -> error "Agda communication failed."
    Just outputs -> do
          putStrLn "Reading file"
          fileContents <- readFile file
          putStrLn $ show outputs
          let ins = sortAndSkipExtended outputs
          --putStrLn $  unlines $ foldl doInstructions (lines fileContents) ins
          --System.IO.putStrLn $ "Test finished: "
          hFlush stdout
          --ts <- action file (hin, hout, processHandle)
          terminateProcess processHandle
          return $  unlines $ foldl doInstructions (lines fileContents) ins

readUntilLast :: Handle -> IO [ChangeIns]
readUntilLast h = do
    line <- hGetLine h
    putStrLn "in readUntilLast"
    putStrLn line
    hFlush stdout
    if isPrefixOf "(agda2-info-action \"*All" line
      then case parse parseGoalsWarningsErrors "Agda output" line of
                          Left x  -> error $ errorBundlePretty x
                          Right y -> return y
      else if isPrefixOf "Agda2> (agda2-info-action \"*Error*\"" line
                 || isPrefixOf "((last ." line
                 || isPrefixOf "(agda2-info-action \"*Error*\"" line
            then error line
            else readUntilLast h

instructions :: ParsecT Void String Identity [ChangeIns]
instructions = do
    try parseErrors <|> parseGoalsAndErrors

parseGoalsWarningsErrors :: ParsecT Void String Identity [ChangeIns]
parseGoalsWarningsErrors = do
    skipSomeTill printChar $ string "*\" \""
    many (try goalParser)
    many $ try warningParser
    parseErrors


parseErrors :: ParsecT Void String Identity [ChangeIns]
parseErrors = do
    skipSomeTill printChar $ many (try incompletePatternParser) -- $ string "\" nil)"

parseGoalsAndErrors :: ParsecT Void String Identity [ChangeIns]
parseGoalsAndErrors = do
  string "(agda2-info-action \"*All Goals, Errors*\" \""
  someTill (try goalParser) $ string "\\n——"
  skipSomeTill (printChar) (string "\\n")
  many (try incompletePatternParser) -- $ string "\" nil)"

incompletePatternParser :: ParsecT Void String Identity ChangeIns
incompletePatternParser = do
       skipManyTill printChar (char ':')
       line <- some digitChar
       char ','
       column <- some digitChar
       char '-'
       some digitChar
       skipManyTill printChar (try $ string "Missing\\ncases:\\n"
              <|> string "Missing cases:\\n")
       newStuff <- many $ do
          string "  "
          manyTill printChar (string "\\n")
       return $ ChangeIns newStuff (read line) (read column)

goalParser :: ParsecT Void String Identity ()
goalParser = do
    string "?"
    skipManyTill printChar (string "\\n")
    return ()

warningParser :: ParsecT Void String Identity ()
warningParser = do
  skipManyTill printChar (string " Warnings")
  skipManyTill printChar (string "\\n\\n")
  return ()

sortAndSkipExtended :: [ChangeIns] -> [ChangeIns]
sortAndSkipExtended input =  filtering $ sortBy sorting input
    where sorting (ChangeIns _ x _) (ChangeIns _ y _) = compare y x
          filtering :: [ChangeIns] -> [ChangeIns]
          filtering = map (\(ChangeIns x a b) ->
                            ChangeIns
                              (filter (\y -> not $ isPrefixOf ".extendedlambda" y) x)
                              a b)

doInstructions :: [String] -> ChangeIns -> [String]
doInstructions code (ChangeIns newStuff line column) =
  let (before , after) = splitAt (line - 1) code
  in before ++ (map (\x -> replicate (column - 1) ' ' ++ x ++ " = ?") newStuff) ++ after
