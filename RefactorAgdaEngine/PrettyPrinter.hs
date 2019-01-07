{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyPrinter (prettyPrint, prettyPrintAll, printForAgda, printExprForAgda) where
import qualified Data.List                    as List
import           Data.Text                    (Text, append, intercalate, pack)
import qualified Data.Text                    as T
import           ParseTree
import           Text.PrettyPrint.Leijen.Text as P

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
dealWithLongLine = hang indentationWidth . fillSep

printParseTree :: ParseTree -> Doc
printParseTree Signature {signature} = hang indentationWidth $ printSignature signature
printParseTree FunctionDefinition {definitionOf, params, body} =
  dealWithLongLine $ printIdentifier definitionOf ++ map (\x -> bracketing x False $ printExpr x) params ++ [textStrict "=", printExpr body]
printParseTree DataStructure {dataName, parameters, indexInfo, constructors, comments} =
  dealWithLongLine
      ((textStrict "data": printIdentifier dataName) ++
      map (parens . hang indentationWidth . printSignature) parameters ++
      [textStrict ":", printExpr indexInfo, textStrict "where"] ++ map printComment (concat comments))
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
  fillSep $ fillSep (printIdentifier funcName ++  [textStrict":"]) : [printExpr funcType]

bracketing :: Expr -> Bool -> (Doc -> Doc)
bracketing FunctionApp {isType} b   = if isType == b then parens else id
bracketing _                      b = id

printExpr :: Expr -> Doc
printExpr expr =
  case expr of
    NumLit {value} -> integer value
    Ident {identifier} -> fillSep $ printIdentifier identifier
    Hole {textInside} -> enclose (textStrict "{!") (textStrict "!}") $ textStrict textInside
    NamedArgument {arg, explicit} ->
      case explicit of
        True  -> parens $ printSignature arg
        False -> braces $ printSignature arg
    FunctionApp firstPart secondPart False->  printExpr firstPart </> bracketing secondPart False (printExpr secondPart)
    FunctionApp input output True ->
        (bracketing input True (printExpr input)) </> arrow </> printExpr output
    Implicit e -> braces $ printExpr e
    Underscore pos bef aft -> fillSep $ map printComment bef ++ textStrict "_" : map printComment aft

printIdentifier :: Identifier -> [Doc]
printIdentifier Identifier{name, commentsBefore, commentsAfter} = (map printComment commentsBefore) ++ textStrict name : (map printComment commentsAfter)

printPragma :: Pragma -> Doc
printPragma Builtin {concept, definition}=
  enclose (textStrict "{-# ") (textStrict " #-}") $ fillSep $
  [textStrict "BUILTIN", textStrict concept] ++ printIdentifier definition
printPragma (Option opts) =
  enclose (textStrict "{-# ") (textStrict " #-}") $ fillSep $
  [textStrict "OPTIONS"] ++ map textStrict opts

printComment :: Comment -> Doc
printComment Comment {content, codePos, isMultiLine} =
    case isMultiLine of
      True  -> enclose (textStrict"{-") (textStrict"-}") $ textStrict content
      False -> (textStrict $ append "--" content) <> line
