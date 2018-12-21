{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser (Parser.parse, Parser, functionType, typeSignature) where
import           Brackets
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Functor.Identity
import           Data.List                      (intersperse)
import           Data.Text                      as T
import           Data.Void
import           ParseTree
import           Text.Megaparsec                as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Read                      (readMaybe)

-- Stuff which is important for the whole file
-- Naming convention: the space consumer for line folds is always called sc.

parse :: Text -> String -> [ParseTree]
parse s fileName = case M.parse manyDefs fileName s of
                    Left x  -> error $ errorBundlePretty x
                    Right y -> y

type Parser = ParsecT Void Text Identity

makeIdentifier :: Text -> (Integer -> RangePosition) -> Bool -> Identifier
makeIdentifier name range inScope = Identifier name range 0 0 inScope [] []

makeSingleRangeFunction :: Int -> Int -> (Integer -> RangePosition)
makeSingleRangeFunction lastUnaffected lastAffected x =
  if x > (toInteger lastAffected) then After
    else if x < toInteger lastUnaffected then Before else Inside
  --x >= toInteger lastUnaffected && x <= toInteger lastAffected + 1

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
  a <- getOffset
  string "{-#"
  space
  prag <- try builtinPragma <|> optionsPragma
  string "#-}"
  z <- getOffset
  return $ Pragma prag $ makeRange a z

builtinPragma :: Parser Pragma
builtinPragma = do
    string "BUILTIN"
    space1
    concept <- some upperChar
    space1
    definition <- ident
    space1
    return $ Builtin (pack concept) definition

optionsPragma :: Parser Pragma
optionsPragma = do
  string "OPTIONS"
  space1
  opts <- sepEndBy option space1
  return $ Option opts
  where option :: Parser Text
        option = do
            string "--"
            opt <- some $ letterChar <|> char '-'
            return $ append "--" $ pack opt

signatureParser :: Parser ParseTree
signatureParser = do
  a <- getOffset
  content <- L.lineFold skipTopLevelComments typeSignature
  z <- getOffset
  return $ Signature content $ makeRange a z

dataStructureParser :: Parser ParseTree
dataStructureParser = do
  a <- getOffset
  let parseEmptyDataStructure = L.lineFold skipTopLevelComments dataDefinition
  let item = L.lineFold skipTopLevelComments typeSignature
  (emptyDataStructure, constructors) <-
    doBlockLikeStructure parseEmptyDataStructure item False skipTopLevelComments
  z <- getOffset
  return $ emptyDataStructure {constructors = constructors, range = makeRange a z}

functionDefinitionParser :: Parser ParseTree
functionDefinitionParser = lineFoldAndRange functionDefinition

openImportParser :: Parser ParseTree
openImportParser = lineFoldAndRange openImport

moduleNameParser :: Parser ParseTree
moduleNameParser = lineFoldAndRange parseModuleName

lineFoldAndRange :: (Parser () -> Parser ParseTree) -> Parser ParseTree
lineFoldAndRange parser = do
  a <- getOffset
  content <- L.lineFold skipTopLevelComments parser
  z <- getOffset
  return $ content{range = makeRange a z}

-- parsing open and import statements
openImport :: Parser () -> Parser ParseTree
openImport sc = try openAndImport <|> try importOnly <|> openOnly
  where openOnly = do string "open"
                      c <- allCommentsUntilNonComment
                      sc
                      name <- qualifiedName
                      return OpenImport { opened = True, imported = False, moduleName = name, comments = [c , []]}
        importOnly = do
                            string "import"
                            c <- allCommentsUntilNonComment
                            sc
                            name <- qualifiedName
                            return OpenImport { opened = False, imported = True, moduleName = name, comments = [[] , c]}
        openAndImport = do
          string "open"
          c <- allCommentsUntilNonComment
          sc
          string "import"
          d <- allCommentsUntilNonComment
          sc
          name <- qualifiedName
          return OpenImport { opened = True, imported = True, moduleName = name
                    , comments = [c , d]}


parseModuleName :: Parser () -> Parser ParseTree
parseModuleName sc = do
  string "module"
  c <- allCommentsUntilNonComment
  sc
  name <- qualifiedName
  d <- allCommentsUntilNonComment
  sc
  string "where"
  return ModuleName {moduleName = name {commentsBefore = c , commentsAfter = d}}

-- Function definition parsing
functionDefinition :: Parser () -> Parser ParseTree
functionDefinition sc = do
  definitionOf <- ident
  c <- allCommentsUntilNonComment
  sc
  params <- endBy (parameter sc) sc
  string "="
  sc
  body <- functionApp sc
  return $ FunctionDefinition definitionOf params body $ Range (-1) (-1)

attachCommentsAfter :: [Comment] -> Expr -> Expr
attachCommentsAfter cs (FunctionApp f x b) =
    FunctionApp f (attachCommentsAfter cs x) b
attachCommentsAfter cs (Ident identifier) =
    Ident (commentsAfterIdent cs identifier)
attachCommentsAfter cs (a @ NamedArgument {arg}) = a {arg = addToId $ arg}
      where addToId (TypeSignature n t) = TypeSignature n $ attachCommentsAfter cs t
attachCommentsAfter cs x = x {commentsAf = commentsAf x ++ cs}

attachCommentsBefore :: [Comment] -> Expr -> Expr
attachCommentsBefore cs (FunctionApp f x b) =
    FunctionApp (attachCommentsAfter cs f) x b
attachCommentsBefore cs (Ident identifier) =
    Ident (commentsBeforeIdent cs identifier)
attachCommentsBefore cs (a @ NamedArgument {arg}) = a {arg = addToId $ arg}
      where addToId (TypeSignature n t) = TypeSignature
               (commentsBeforeIdent cs n) t
attachCommentsBefore cs x = x {commentsBef = cs ++ commentsBef x}

commentsBeforeIdent :: [Comment] -> Identifier -> Identifier
commentsBeforeIdent cs i = i {commentsBefore = cs ++ commentsBefore i}

commentsAfterIdent :: [Comment] -> Identifier -> Identifier
commentsAfterIdent cs i = i {commentsAfter = commentsAfter i ++ cs}

parameter :: Parser () -> Parser Expr
parameter sc = try ( do x <- ident
                        c <- allCommentsUntilNonComment
                        return $ attachCommentsAfter c $ Ident x
                   ) <|>
               do -- unlike normal function application, this needs to be
               -- wrapped in parentheses
                 string "("
                 c <- allCommentsUntilNonComment
                 sc
                 x <- functionApp sc
                 d <- allCommentsUntilNonComment
                 sc
                 string ")"
                 return $ attachCommentsBefore c $ attachCommentsAfter d x



-- data structure parsing
dataDefinition :: Parser () -> Parser ParseTree
dataDefinition sc = do
  string "data"
  c <- allCommentsUntilNonComment
  sc
  name <- ident
  d <- allCommentsUntilNonComment
  sc
  parameters <- sepEndBy sign sc
  --sc
  string ":"
  e <- allCommentsUntilNonComment
  sc
  indexInfo <- functionType sc
  sc
  f <- allCommentsUntilNonComment
  sc
  string "where"
  g <- allCommentsUntilNonComment
  return $ DataStructure
              (commentsAfterIdent d $ commentsBeforeIdent c name)
              parameters
              (attachCommentsBefore e $ attachCommentsAfter f indexInfo) [] (Range (-1) (-1)) [g]
  where sign = do
          string "("
          c <- allCommentsUntilNonComment
          sc
          x <- typeSignature sc
          let (TypeSignature funcName funcType ) = x
          d <- allCommentsUntilNonComment
          sc
          string ")"
          e <- allCommentsUntilNonComment
          return $ TypeSignature
                    (commentsBeforeIdent c funcName)
                    $ attachCommentsAfter (d ++ e) funcType

-- Type signature parsing

typeSignature :: Parser () -> Parser TypeSignature
typeSignature sc = do
 name <- ident
 c <- allCommentsUntilNonComment
 sc
 string ":"
 d <- allCommentsUntilNonComment
 sc
 kind <- functionType sc
 --sc
 return $ TypeSignature (commentsAfterIdent c name) $ attachCommentsBefore d kind

-- Expression parsing

hole :: Parser Expr
hole = questionMark <|> do
      a <- getOffset
      c <- flatNestedStructure "{!" "!}"
        (\x -> Hole {textInside = x , commentsBef = [] , commentsAf = []})
         textInside
      z <- getOffset
      af <- allCommentsUntilNonComment
      return $ c {position = makeRange a z , commentsAf = af}
      where questionMark :: Parser Expr
            questionMark = do
              a <- getOffset
              string "?"
              z <- getOffset
              return $ Hole "{! !}" (makeRange a z) [] []

literal :: Parser Expr
literal = do
  a <- getOffset
  x <- potentialNamePart
  z <- getOffset
  af <- allCommentsUntilNonComment
  case (readMaybe $ unpack x) :: Maybe Integer of
    Nothing -> fail $ unpack x ++ "is not an int literal"
    Just y  -> return $ NumLit y (makeRange a z)  [] af

identExpr :: Parser Expr
identExpr = do
  x <- ident
  af <- allCommentsUntilNonComment
  return $ attachCommentsAfter af $ Ident x

-- {Comment 1} f {comment2} x {comment3}
functionApp :: Parser () -> Parser Expr
functionApp sc = do
   bef <- allCommentsUntilNonComment

   e <- makeExprParserWithParens
                 sc
                 (try literal <|> try identExpr <|>
                    try ( (\x -> Ident $ makeOutOfScope x) <$> (string "." >> ident)) <|> hole)
                 operatorTable
   return $ attachCommentsBefore bef e
      where whitespaceAsOperator :: Parser Expr
            whitespaceAsOperator = do
                sc
                lookAhead $ try $ functionApp sc
            operatorTable ::  [[Operator Parser Expr]]
            operatorTable = [[InfixL $ (\ x y -> FunctionApp x y False) <$ try whitespaceAsOperator]]
            makeOutOfScope a = a {inScope = False}

typeParser :: Parser () -> Parser Expr
typeParser sc =
    functionApp sc


functionType :: Parser () -> Parser Expr
functionType sc =
  makeExprParserWithParens
  sc
  (try (functionApp sc) <|> try (implicitArgParser sc) <|> explicitArgParser sc)
  arrowTable
  where arrowTable :: [[Operator Parser Expr]]
        arrowTable = [[InfixR $ (\ x y -> FunctionApp x y True) <$ try arrowParser]]
        arrowParser :: Parser ()
        arrowParser = do
          sc
          string "→" <|> string "->"
          sc



implicitArgParser :: Parser () -> Parser Expr
implicitArgParser = anyArgParser "{" "}" False

explicitArgParser :: Parser () -> Parser Expr
explicitArgParser = anyArgParser "(" ")" True

anyArgParser :: Text -> Text -> Bool -> Parser () -> Parser Expr
anyArgParser opening closing b sc = do
  string opening
  sc
  x <- typeSignature sc
  sc
  string closing
  c <- allCommentsUntilNonComment
  return $ NamedArgument x b [] c



-- Identifier parsing
ident :: Parser Identifier
ident = identInScope <|> identNotInScope

identInScope :: Parser Identifier
identInScope = do
  lastBefore <- getOffset
  x <- namePart
  lastToken <- getOffset
  return $ makeIdentifier x (makeSingleRangeFunction lastBefore lastToken) True

identNotInScope :: Parser Identifier
identNotInScope = do
      lastBefore <- getOffset
      string "."
      x <- namePart
      lastToken <- getOffset
      return $ makeIdentifier x (makeSingleRangeFunction lastBefore lastToken) False

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
  lastBefore <- getOffset
  x <- sepBy namePart (string ".")
  lastToken <- getOffset
  return $ makeIdentifier (T.concat $ Data.List.intersperse "." x)
      (makeSingleRangeFunction lastBefore lastToken) True

potentialNamePart :: Parser Text
potentialNamePart = do
   chars <- some requirement
   return $ pack chars
   where requirement :: Parser Char
         requirement = satisfy req
         req :: Char -> Bool
         req x = isPrint x && notElem x (" @.(){};_\"\\"::String)

-- Comment parsing

allCommentsUntilNonComment :: Parser [Comment]
allCommentsUntilNonComment = lookAhead $ space >> sepEndBy (lineComment <|> multiLineComment) space

lineComment :: Parser Comment
lineComment = do
  a <- getOffset
  string "--"
  content <- many $ satisfy (\x -> notElem x ("\r\n"::String))
  z <- getOffset
  return $ Comment (pack content) (makeRange a z) False

multiLineComment :: Parser Comment
multiLineComment = do
  x <- observing $ lookAhead $ try $ string "{-#"
  case x of
    Right _ -> fail "Found start of pragma"
    _ -> do
          a <- getOffset
          c <- flatNestedStructure "{-" "-}"
                    (\x -> Comment {content = x })
                    content
          z <- getOffset
          return $ c {codePos = makeRange a z , isMultiLine = True}

flatNestedStructure :: Text -> Text -> (Text -> a) -> (a -> Text)-> Parser a
flatNestedStructure start end constructor deconstructor =
    constructor . T.concat <$> (p >> manyTill e n)
    where e = (\x -> append start $ append (deconstructor x) end ) <$> flatNestedStructure start end constructor deconstructor <|> f
          f :: Parser Text
          f = singleton <$> anySingle
          p = string start
          n = string end

-- Comment skipping
skipTopLevelComments :: Parser ()
skipTopLevelComments =
  L.space space1 (L.skipLineComment "--") $ multiLineComment >> return ()
