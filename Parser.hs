{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser (Parser.parse) where
import Text.Megaparsec as M
import Data.Void
import Data.Functor.Identity
import Text.Megaparsec.Char
import Data.Char
import ParseTree
import Brackets
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text as T
import Text.Read (readMaybe)

-- Stuff which is important for the whole file
-- Naming convention: the space consumer for line folds is always called sc.

parse :: Text -> [ParseTree]
parse s = case M.parse manyDefs "File name for error messages" s of
            Left x -> error $ parseErrorPretty x
            Right y -> y

type Parser = ParsecT Void Text Identity

-- Producing a parse tree

noIndent :: Parser ParseTree
noIndent = L.nonIndented skipTopLevelComments $
      (try pragmaParser) <|> (try dataStructureParser)
      <|> (try signatureParser) <|> (try openImportParser)<|>
      (try moduleNameParser) <|> functionDefinitionParser

manyDefs :: Parser [ParseTree]
manyDefs = do skipTopLevelComments -- remove space at top of file
              x <- sepEndBy noIndent skipTopLevelComments
              eof
              return x

pragmaParser :: Parser ParseTree
pragmaParser = do
  a <- getTokensProcessed
  string "{-#"
  space
  string "BUILTIN"
  space1
  concept <- some upperChar
  space1
  definition <- ident
  space1
  string "#-}"
  z <- getTokensProcessed
  return $ Pragma (Builtin (pack concept) definition) $ Range (toInteger a) (toInteger z) False

signatureParser :: Parser ParseTree
signatureParser = do
  a <- getTokensProcessed
  content <- L.lineFold skipTopLevelComments typeSignature
  z <- getTokensProcessed
  return $ Signature content $ Range (toInteger a) (toInteger z) False

dataStructureParser :: Parser ParseTree
dataStructureParser = do
  a <- getTokensProcessed
  content <- L.indentBlock skipTopLevelComments constructorBlock
  z <- getTokensProcessed
  return $ content{range = Range (toInteger a) (toInteger z) False}

functionDefinitionParser :: Parser ParseTree
functionDefinitionParser = lineFoldAndRange functionDefinition

openImportParser :: Parser ParseTree
openImportParser = lineFoldAndRange openImport

moduleNameParser :: Parser ParseTree
moduleNameParser = lineFoldAndRange parseModuleName

lineFoldAndRange :: (Parser () -> Parser ParseTree) -> Parser ParseTree
lineFoldAndRange parser = do
  a <- getTokensProcessed
  content <- L.lineFold skipTopLevelComments parser
  z <- getTokensProcessed
  return $ content{range = Range (toInteger a) (toInteger z) False}

-- parsing open and import statements
openImport :: Parser () -> Parser ParseTree
openImport sc = try openAndImport <|> try importOnly <|> openOnly
  where openOnly = do string "open"
                      sc
                      name <- qualifiedName
                      return OpenImport { opened = True, imported = False, moduleName = name}
        importOnly = do
                            string "import"
                            sc
                            name <- qualifiedName
                            return OpenImport { opened = False, imported = True, moduleName = name}
        openAndImport = do
          string "open"
          sc
          string "import"
          sc
          name <- qualifiedName
          return OpenImport { opened = True, imported = True, moduleName = name}


parseModuleName :: Parser () -> Parser ParseTree
parseModuleName sc = do
  string "module"
  sc
  name <- qualifiedName
  sc
  string "where"
  return ModuleName {moduleName = name}

-- Function definition parsing
functionDefinition :: Parser () -> Parser ParseTree
functionDefinition sc = do
  definitionOf <- namePart
  sc
  params <- endBy (parameter sc) sc

  string "="
  sc
  body <- functionApp sc
  return $ FunctionDefinition definitionOf params body $ Range (-1) (-1) False

parameter :: Parser () -> Parser Expr
parameter sc = (try ident) <|>
               do -- unlike normal function application, this needs to be
               -- wrapped in parentheses
                 string "("
                 sc
                 x <- functionApp sc
                 sc
                 string ")"
                 return x



-- data structure parsing
dataDefinition :: Parser () -> Parser ParseTree
dataDefinition sc = do
  string "data"
  sc
  name <- namePart
  sc
  parameters <- sepEndBy sign sc
  sc
  string ":"
  sc
  indexInfo <- functionType sc
  sc
  string "where"
  return $ DataStructure name parameters indexInfo [] $ Range (-1) (-1) False
  where sign = do
          string "("
          sc
          x <- typeSignature sc
          sc
          string ")"
          return x

-- The general idea is to first parse the definition part of the
-- data declaration and then use it as the reference point
-- beyond which the constructors must be indented.
constructorBlock :: Parser (L.IndentOpt Parser ParseTree TypeSignature)
constructorBlock = do
  dataType <- dataDefinition space
  return $ L.IndentMany Nothing (\x -> return $ dataType{constructors=x}) $ L.lineFold space typeSignature

-- Type signature parsing

typeSignature :: Parser () -> Parser TypeSignature
typeSignature sc = do
 name <- namePart
 comment1 <- allCommentsUntilNonComment
 sc
 string ":"
 comment2 <- allCommentsUntilNonComment
 sc
 kind <- functionType sc
 return $ TypeSignature name kind $ comment1 : comment2 : []

-- Expression parsing

ident :: Parser Expr
ident = namePart >>= return . Ident

hole :: Parser Expr
hole = questionMark <|>
      flatNestedStructure "{!" "!}" Hole textInside
      where questionMark :: Parser Expr
            questionMark = do
              string "?"
              return $ Hole "{! !}"

numLit :: Parser Expr
numLit = do
  x <- potentialNamePart
  case (readMaybe $ unpack x) :: Maybe Integer of
    Nothing -> fail $ unpack x ++ "is not an int literal"
    Just y -> return $ NumLit y

functionApp :: Parser () -> Parser Expr
functionApp sc = makeExprParserWithParens
                 sc
                 ((try numLit) <|> ident <|> hole)
                 operatorTable
      where whitespaceAsOperator :: Parser Expr
            whitespaceAsOperator = do
                sc
                lookAhead $ try $ functionApp sc
            operatorTable ::  [[Operator Parser Expr]]
            operatorTable = [[InfixL $ FunctionApp <$ try whitespaceAsOperator]]

-- Type parsing

functionType :: Parser () -> Parser Type
functionType sc =
  makeExprParserWithParens
  sc
  (try (typeParser sc) <|> try (implicitArgParser sc) <|> (explicitArgParser sc))
  arrowTable
  where arrowTable :: [[Operator Parser Type]]
        arrowTable = [[InfixR $ FunctionType <$ try arrowParser]]
        --this one now returns failure on a -> \na, therefore causing only a to be parsed.
        --Hopefully resolved when parsers are combined.
        arrowParser :: Parser ()
        arrowParser = do
          sc
          string "->"
          sc

typeParser :: Parser () -> Parser Type
typeParser sc = functionApp sc >>= return . Type

implicitArgParser :: Parser () -> Parser Type
implicitArgParser = anyArgParser "{" "}" ImplicitArgument

explicitArgParser :: Parser () -> Parser Type
explicitArgParser = anyArgParser "(" ")" ExplicitArgument

anyArgParser :: Text -> Text -> (TypeSignature -> Type) -> Parser () -> Parser Type
anyArgParser opening closing constructor sc = do
  string opening
  sc
  x <- typeSignature sc
  sc
  string closing
  return $ constructor x



-- Identifier parsing
namePart :: Parser Text
namePart = do
  x <- potentialNamePart
  if elem x  keywords || isPrefixOf "--" x
  then fail $ (unpack x) ++ " is a reserved word and can't be used as a name part."
  else return x

keywords :: [Text]
keywords = T.words " = | -> : ? \\ → ∀ λ abstract constructor data field forall hiding import in infix infixl infixr let module mutual open postulate primitive Prop private public quoteGoal quoteTerm quote record renaming rewrite syntax unquote using where with "

qualifiedName :: Parser [Text]
qualifiedName = sepBy namePart (string ".")

potentialNamePart :: Parser Text
potentialNamePart = do
   chars <- some requirement
   return $ pack chars
   where requirement :: Parser Char
         requirement = satisfy req
         req :: Char -> Bool
         req x = isPrint x && not (elem x (" @.(){};_"::String))

-- Comment parsing

allCommentsUntilNonComment :: Parser [Comment]
allCommentsUntilNonComment = lookAhead $ space >> sepEndBy (lineComment <|> multiLineComment) space

lineComment :: Parser Comment
lineComment = do
  string "--"
  content <- many $ satisfy (\x -> not (elem x ("\r\n"::String)))
  return $ LineComment $ pack content

multiLineComment :: Parser Comment
multiLineComment = do
  x <- observing $ lookAhead $ try $ string "{-#"
  case x of
    Right _ -> fail "Found start of pragma"
    _ -> flatNestedStructure "{-" "-}" MultiLineComment content

flatNestedStructure :: Text -> Text -> (Text -> a) -> (a -> Text)-> Parser a
flatNestedStructure start end constructor deconstructor =
  (p >> manyTill e n) >>= return . constructor . T.concat
    where e = (flatNestedStructure start end constructor deconstructor >>= return . (\x -> append start $ append (deconstructor x) end )) <|> f
          f :: Parser Text
          f = anyChar >>= return . singleton
          p = string start
          n = string end

-- Comment skipping
skipTopLevelComments :: Parser ()
skipTopLevelComments =
  L.space space1 (L.skipLineComment "--") $ multiLineComment >> return ()
