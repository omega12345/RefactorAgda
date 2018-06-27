{-# LANGUAGE OverloadedStrings #-}

module ParseHaskellData where
import Text.Megaparsec as M
import Data.Text
import Text.Megaparsec.Char
import Data.Void
import Data.Functor.Identity
import qualified Text.Megaparsec.Char.Lexer as L

data DataStructure = NameAndAllConstructors {name ::Text
                                            ,cons ::[Constructor]
                                            } deriving Show

data Constructor = Constructor {consname :: Text
                               ,contents :: [(Text, [Text])]
                                } deriving Show

-- | parses all Haskell data structures in input, ignoring everything else.
parse :: Text -> [DataStructure]
parse s = case M.parse pickOutData "File name for error messages" s of
              Left x -> error $ parseErrorPretty x
              Right y -> y

type Parser = ParsecT Void Text Identity

pickOutData :: Parser [DataStructure]
pickOutData = do
              notData
              x <- endBy dataParser notData
              eof
              return x

dataParser :: Parser DataStructure
dataParser = do
  string "data"
  space
  name <- some alphaNumChar
  space
  string "="
  space
  constructors <- sepBy constructorParser $ string "|"
  return $ NameAndAllConstructors (pack name) constructors

constructorParser :: Parser Constructor
constructorParser = do
  space
  name <- some alphaNumChar
  space
  string "{"
  space
  contents <- sepBy single $ string ","
  --space
  string "}"
  space
  return $ Constructor (pack name) contents

  where single :: Parser (Text, [Text])
        single = do
          space
          name <- some alphaNumChar
          space
          string "::"
          space
          x <- sepBy (L.lexeme space $ some (alphaNumChar <|> char '[' <|> char ']')) (string "->" >> space)
          space
          return (pack name, Prelude.map pack x)

notData :: Parser ()
notData = skipManyTill (spaceChar <|> printChar) (lookAhead (string "data" >> return ()) <|> eof)
