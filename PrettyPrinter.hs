{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyPrinter (prettyPrint, prettyPrintAll) where
import Data.Text (Text, append, pack, intercalate)
import qualified Data.Text as T
import qualified Data.List as List
import ParseTree
import Text.PrettyPrint.Leijen.Text as P

prettyPrint :: [ParseTree] -> [ParseTree] -> Text -> Text
prettyPrint newTree oldTree oldCode = foldr (replaceSection False) oldCode (List.zip oldTree newTree)

prettyPrintAll :: [ParseTree] -> [ParseTree] -> Text -> Text
prettyPrintAll newTree oldTree oldCode = foldr (replaceSection True) oldCode (List.zip oldTree newTree)

replaceSection :: Bool -> (ParseTree, ParseTree) -> Text -> Text
replaceSection replaceAll (oldCode, newCode) inText =
  let r = range newCode
      before = T.take (fromInteger $ lastUnaffected r) inText
      after = T.drop (fromInteger $ lastAffected r) inText
  in if (oldCode /= newCode) || replaceAll
     then T.concat [before, render newCode, after]
     else inText

maxLineLength = 80
indentationWidth = 4
arrow = textStrict "->"

render :: ParseTree -> Text
render p = displayTStrict $ renderPretty 1 maxLineLength $ printParseTree p

-- hanging indent: All following lines are more indented than the first
-- fillSep: Concatenates input on one line as long as it will fit and then inserts a line break

dealWithLongLine :: [Doc] -> Doc
dealWithLongLine = hang indentationWidth . sep

printParseTree :: ParseTree -> Doc
printParseTree Signature {signature} = hang indentationWidth $ printSignature signature
printParseTree FunctionDefinition {definitionOf, params, body} =
  dealWithLongLine $ [printIdentifier definitionOf] ++ map printParameter params ++ [textStrict "=", printExpr body]
printParseTree DataStructure {dataName, parameters, indexInfo, constructors} =
  dealWithLongLine
      ([textStrict "data", printIdentifier dataName] ++
      map (parens . hang indentationWidth . printSignature) parameters ++
      [textStrict ":", dealWithLongLine $ printType indexInfo False, textStrict "where"])
       P.<$>
  vsep ( map (indent indentationWidth . hang indentationWidth . printSignature) constructors)
printParseTree Pragma {pragma} = printPragma pragma
printParseTree OpenImport { opened, imported, moduleName} =
  dealWithLongLine
  [if opened then textStrict "open" else empty, if imported then textStrict "import" else empty,
  printIdentifier moduleName]
printParseTree ModuleName { moduleName} =
  dealWithLongLine [textStrict "module", printIdentifier moduleName , textStrict "where"]

--TODO: The rest of the comments does not get printed because right now it is guaranteed to be 0.
printSignature :: TypeSignature -> Doc
printSignature TypeSignature {funcName, funcType, comments = (x:y:rest)} =
  sep $ sep ([printIdentifier funcName] ++ map printComment x ++ [textStrict":"] ++ map printComment y) : printType funcType False

printExpr :: Expr -> Doc
printExpr expr =
  case expr of
    ExprLit {literal} -> printLiteral literal
    Hole {textInside} -> enclose (textStrict "{!") (textStrict "!}") $ textStrict textInside
    FunctionApp {function, argument} -> printExpr function </> bracketing argument (printExpr argument)
  where bracketing (FunctionApp _ _) = parens
        bracketing _ = id

printIdentifier :: Identifier -> Doc
printIdentifier Identifier{name} = textStrict name

printParameter :: Parameter -> Doc
printParameter ParamApp {paramFunc , paramArg} =
            parens $ printParameter paramFunc </> printParameter paramArg
printParameter (Lit literal) = printLiteral literal

printLiteral :: IdentOrLiteral -> Doc
printLiteral NumLit {value} = integer value
printLiteral (Ident x) = printIdentifier x

printType :: Type -> Bool -> [Doc]
printType t wantBrackets =
  case t of
    Type {expression} -> [printExpr expression]
    ImplicitArgument { impArg} ->
      [braces $ printSignature impArg]
    ExplicitArgument { expArg} ->
      [parens $ printSignature expArg]
    FunctionType { input, output } ->
      if wantBrackets
      then [parens $ sep temp]
      else temp
      where temp = (sep (printType input True) </> arrow) : printType output False

printPragma :: Pragma -> Doc
printPragma Builtin {concept, definition}=
  enclose (textStrict "{-# ") (textStrict " #-}") $ sep
  [textStrict "BUILTIN", textStrict concept, printIdentifier definition]

printComment :: Comment -> Doc
printComment LineComment {content} = textStrict $ append "--" content
printComment MultiLineComment {content} = enclose (textStrict"{-") (textStrict"-}") $ textStrict content
