{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser (Parser.parse, Parser) where
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
import Data.List (intersperse)

-- Stuff which is important for the whole file
-- Naming convention: the space consumer for line folds is always called sc.

parse :: Text -> [ParseTree]
parse s = case M.parse manyDefs "File name for error messages" s of
            Left x -> error $ parseErrorPretty x
            Right y -> y

type Parser = ParsecT Void Text Identity

makeIdentifier :: Text -> (Integer -> Bool) -> Identifier
makeIdentifier name range = Identifier name range 0 0

makeSingleRangeFunction :: Int -> Int -> (Integer -> Bool)
makeSingleRangeFunction lastUnaffected lastAffected x =
  x >= toInteger lastUnaffected && x <= toInteger lastAffected + 1

makeRange :: Int -> Int -> Range
makeRange a z = Range (toInteger a) (toInteger z)

-- Producing a parse tree

noIndent :: Parser ParseTree
noIndent = L.nonIndented skipTopLevelComments $
      try pragmaParser <|> try dataStructureParser
      <|> try signatureParser <|> try openImportParser<|>
      try moduleNameParser <|> functionDefinitionParser

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
  return $ Pragma (Builtin (pack concept) definition) $ makeRange a z

signatureParser :: Parser ParseTree
signatureParser = do
  a <- getTokensProcessed
  content <- L.lineFold skipTopLevelComments typeSignature
  z <- getTokensProcessed
  return $ Signature content $ makeRange a z

dataStructureParser :: Parser ParseTree
dataStructureParser = do
  a <- getTokensProcessed
  let parseEmptyDataStructure = L.lineFold skipTopLevelComments dataDefinition
  let item = L.lineFold space typeSignature
  (emptyDataStructure, constructors) <-
    doBlockLikeStructure parseEmptyDataStructure item False skipTopLevelComments
  z <- getTokensProcessed
  return $ emptyDataStructure {constructors = constructors, range = makeRange a z}

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
  return $ content{range = makeRange a z}

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
  definitionOf <- ident
  sc
  params <- endBy (parameter sc) sc
  string "="
  sc
  body <- functionApp sc
  return $ FunctionDefinition definitionOf params body $ Range (-1) (-1)

parameter :: Parser () -> Parser Parameter
parameter sc = try (Lit . Ident <$> ident) <|>
               do -- unlike normal function application, this needs to be
               -- wrapped in parentheses
                 string "("
                 sc
                 x <- paramFunctionApp sc
                 sc
                 string ")"
                 return x



-- data structure parsing
dataDefinition :: Parser () -> Parser ParseTree
dataDefinition sc = do
  string "data"
  sc
  name <- ident
  sc
  parameters <- sepEndBy sign sc
  sc
  string ":"
  sc
  indexInfo <- functionType sc
  sc
  string "where"
  return $ DataStructure name parameters indexInfo [] $ Range (-1) (-1)
  where sign = do
          string "("
          sc
          x <- typeSignature sc
          sc
          string ")"
          return x

-- Type signature parsing

typeSignature :: Parser () -> Parser TypeSignature
typeSignature sc = do
 name <- ident
 comment1 <- allCommentsUntilNonComment
 sc
 string ":"
 comment2 <- allCommentsUntilNonComment
 sc
 kind <- functionType sc
 return $ TypeSignature name kind [comment1, comment2]

-- Expression parsing

hole :: Parser Expr
hole = questionMark <|>
      flatNestedStructure "{!" "!}" Hole textInside
      where questionMark :: Parser Expr
            questionMark = do
              string "?"
              return $ Hole "{! !}"

numLit :: Parser IdentOrLiteral
numLit = do
  x <- potentialNamePart
  case (readMaybe $ unpack x) :: Maybe Integer of
    Nothing -> fail $ unpack x ++ "is not an int literal"
    Just y -> return $ NumLit y

paramFunctionApp :: Parser () -> Parser Parameter
paramFunctionApp sc = makeExprParserWithParens
                 sc
                 (try (Lit <$> numLit) <|> (Lit . Ident <$> ident))
                 operatorTable
      where whitespaceAsOperator :: Parser Parameter
            whitespaceAsOperator = do
                sc
                lookAhead $ try $ paramFunctionApp sc
            operatorTable ::  [[Operator Parser Parameter]]
            operatorTable = [[InfixL $ ParamApp <$ try whitespaceAsOperator]]

functionApp :: Parser () -> Parser Expr
functionApp sc = makeExprParserWithParens
                 sc
                 (try (ExprLit <$> numLit) <|> try ( ExprLit . Ident <$> ident) <|> hole)
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
  (try (typeParser sc) <|> try (implicitArgParser sc) <|> explicitArgParser sc)
  arrowTable
  where arrowTable :: [[Operator Parser Type]]
        arrowTable = [[InfixR $ FunctionType <$ try arrowParser]]
        arrowParser :: Parser ()
        arrowParser = do
          sc
          string "->"
          sc

typeParser :: Parser () -> Parser Type
typeParser sc = Type <$> functionApp sc

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
ident :: Parser Identifier
ident = do
  lastBefore <- getTokensProcessed
  x <- namePart
  lastToken <- getTokensProcessed
  return $ makeIdentifier x (makeSingleRangeFunction lastBefore lastToken)

namePart :: Parser Text
namePart = do
  x <- potentialNamePart
  if elem x  keywords || isPrefixOf "--" x
  then fail $ unpack x ++ " is a reserved word and can't be used as a name part."
  else return x

keywords :: [Text]
keywords = T.words " = | -> : ? \\ → ∀ λ abstract constructor data field forall hiding import in infix infixl infixr let module mutual open postulate primitive Prop private public quoteGoal quoteTerm quote record renaming rewrite syntax unquote using where with "

--TODO: Should have a parser for Set as well

qualifiedName :: Parser Identifier
qualifiedName = do
  lastBefore <- getTokensProcessed
  x <- sepBy namePart (string ".")
  lastToken <- getTokensProcessed
  return $ makeIdentifier (T.concat $ Data.List.intersperse "." x)
      (makeSingleRangeFunction lastBefore lastToken)

potentialNamePart :: Parser Text
potentialNamePart = do
   chars <- some requirement
   return $ pack chars
   where requirement :: Parser Char
         requirement = satisfy req
         req :: Char -> Bool
         req x = isPrint x && notElem x (" @.(){};_"::String)

-- Comment parsing

allCommentsUntilNonComment :: Parser [Comment]
allCommentsUntilNonComment = lookAhead $ space >> sepEndBy (lineComment <|> multiLineComment) space

lineComment :: Parser Comment
lineComment = do
  string "--"
  content <- many $ satisfy (\x -> notElem x ("\r\n"::String))
  return $ LineComment $ pack content

multiLineComment :: Parser Comment
multiLineComment = do
  x <- observing $ lookAhead $ try $ string "{-#"
  case x of
    Right _ -> fail "Found start of pragma"
    _ -> flatNestedStructure "{-" "-}" MultiLineComment content

flatNestedStructure :: Text -> Text -> (Text -> a) -> (a -> Text)-> Parser a
flatNestedStructure start end constructor deconstructor =
    constructor . T.concat <$> (p >> manyTill e n)
    where e = (\x -> append start $ append (deconstructor x) end ) <$> flatNestedStructure start end constructor deconstructor <|> f
          f :: Parser Text
          f = singleton <$> anyChar
          p = string start
          n = string end

-- Comment skipping
skipTopLevelComments :: Parser ()
skipTopLevelComments =
  L.space space1 (L.skipLineComment "--") $ multiLineComment >> return ()
