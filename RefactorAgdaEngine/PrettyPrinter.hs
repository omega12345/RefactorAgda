{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyPrinter (prettyPrint, prettyPrintAll, printForAgda, printExprForAgda) where
import Data.Text (Text, append, pack, intercalate)
import qualified Data.Text as T
import qualified Data.List as List
import ParseTree
import Text.PrettyPrint.Leijen.Text as P

prettyPrint :: [ParseTree] -> [ParseTree] -> Text -> Text
prettyPrint newTree oldTree oldCode = foldr (replaceSection False) oldCode (List.zip oldTree newTree)

prettyPrintAll :: [ParseTree] -> [ParseTree] -> Text -> Text
prettyPrintAll newTree oldTree oldCode = intercalate "\n\n" $ map render newTree

printForAgda :: [ParseTree] -> Text
printForAgda p = intercalate "\n" $ map render p

printExprForAgda :: Expr -> Text
printExprForAgda e = displayTStrict $ renderOneLine $ printExpr e

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
  dealWithLongLine $ printIdentifier definitionOf ++ map (parens . printExpr) params ++ [textStrict "=", printExpr body]
printParseTree DataStructure {dataName, parameters, indexInfo, constructors, comments} =
  dealWithLongLine
      ((textStrict "data": printIdentifier dataName) ++
      map (parens . hang indentationWidth . printSignature) parameters ++
      [textStrict ":", dealWithLongLine $ printType indexInfo False, textStrict "where"] ++ map printComment (concat comments))
       P.<$>
  vsep ( map (indent indentationWidth . hang indentationWidth . printSignature) constructors)
printParseTree Pragma {pragma} = printPragma pragma
printParseTree OpenImport { opened, imported, moduleName, comments} =
  dealWithLongLine $
  (if opened then textStrict "open" : map printComment (head comments) else [empty]) ++
  (if imported then textStrict "import" : map printComment (comments!!1) else [empty]) ++
  printIdentifier moduleName
printParseTree ModuleName { moduleName} =
  dealWithLongLine $ (textStrict "module": printIdentifier moduleName) ++ [ textStrict "where"]

--TODO: The rest of the comments does not get printed because right now it is guaranteed to be 0.
printSignature :: TypeSignature -> Doc
printSignature TypeSignature {funcName, funcType} =
  sep $ sep (printIdentifier funcName ++  [textStrict":"]) : printType funcType False

printExpr :: Expr -> Doc
printExpr expr =
  case expr of
    NumLit {value} -> integer value
    Ident {identifier} -> sep $ printIdentifier identifier
    Hole {textInside} -> enclose (textStrict "{!") (textStrict "!}") $ textStrict textInside
    FunctionApp {function, argument} -> printExpr function </> bracketing argument (printExpr argument)
  where bracketing (FunctionApp _ _) = parens
        bracketing _ = id

printIdentifier :: Identifier -> [Doc]
printIdentifier Identifier{name, commentsBefore, commentsAfter} = (map printComment commentsBefore) ++ textStrict name : (map printComment commentsAfter)

printType :: Type -> Bool -> [Doc]
printType t wantBrackets =
  case t of
    Type {expression} -> [printExpr expression]
    NamedArgument { arg , explicit} ->
      case explicit of
        True -> [parens $ printSignature arg]
        False -> [braces $ printSignature arg]
    FunctionType { input, output } ->
      if wantBrackets
      then [parens $ sep temp]
      else temp
      where temp = (sep (printType input True) </> arrow) : printType output False

printPragma :: Pragma -> Doc
printPragma Builtin {concept, definition}=
  enclose (textStrict "{-# ") (textStrict " #-}") $ sep $
  [textStrict "BUILTIN", textStrict concept] ++ printIdentifier definition
printPragma (Option opts) =
  enclose (textStrict "{-# ") (textStrict " #-}") $ sep $
  [textStrict "OPTIONS"] ++ map textStrict opts

printComment :: Comment -> Doc
printComment Comment {content, codePos, isMultiLine} =
    case isMultiLine of
      True -> enclose (textStrict"{-") (textStrict"-}") $ textStrict content
      False -> textStrict $ append "--" content
