{-# LANGUAGE OverloadedStrings #-}
module TranslateHaskellData where
import System.Environment
import Data.Text.IO as IO
import ParseHaskellData
import PrintAgdaData
import Data.Text
main :: IO ()
main = do
    contents <- IO.readFile "ParseTree.hs"
    IO.writeFile "ParseTree.agda" $ pack $ "module ParseTree where\n" ++
     "open import Data.String\n" ++
     "open import Data.Nat\n" ++
     "open import Data.Bool\n" ++
     "open import Data.List\n" ++
     "{-# FOREIGN GHC import ParseTree #-}\n"
    IO.appendFile "ParseTree.agda" $ toAgda $ parse contents
