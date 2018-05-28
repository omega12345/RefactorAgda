{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyPrinter (prettyPrint) where
import Data.Text (Text, append, pack, intercalate)
import qualified Data.Text as T
import qualified Data.List as List
import ParseTree
import Text.PrettyPrint.Leijen.Text as P

prettyPrint :: [ParseTree] -> Text -> Text
prettyPrint newTree oldCode = foldr replaceSection oldCode newTree

replaceSection :: ParseTree -> Text -> Text
replaceSection section inText =
  let r = range section
      before = T.take (fromInteger $ lastUnaffected r) inText
      after = T.drop (fromInteger $ lastAffected r) inText
  in if (needsUpdating r)
     then (T.concat [before, (render section), after])
     else inText

maxLineLength = 80
indentationWidth = 4
arrow = textStrict "->"

render :: ParseTree -> Text
render p = displayTStrict $ renderPretty 1 maxLineLength $ printParseTree p

-- hanging indent: All following lines are more indented than the first
-- fillSep: Concatenates input on one line as long as it will fit and then inserts a line break

dealWithLongLine :: [Doc] -> Doc
dealWithLongLine = (hang indentationWidth) . sep

printParseTree :: ParseTree -> Doc
printParseTree (Signature {signature}) = hang indentationWidth $ printSignature signature
printParseTree (FunctionDefinition {definitionOf, params, body}) =
  dealWithLongLine $ [textStrict definitionOf] ++ (map addBrackets params) ++ [textStrict "=", printExpr body]
printParseTree (DataStructure {dataName, parameters, indexInfo, constructors}) =
  (dealWithLongLine $
      [textStrict "data", textStrict dataName] ++
      (map (parens . hang indentationWidth . printSignature) parameters) ++
      [textStrict ":", dealWithLongLine $ printType indexInfo False, textStrict "where"]
      ) P.<$>
  (vsep $  map (indent indentationWidth . hang indentationWidth . printSignature) constructors)
printParseTree (Pragma {pragma}) = printPragma pragma
printParseTree (OpenImport { opened, imported, moduleName}) =
  dealWithLongLine $
  [(if opened then textStrict "open" else empty), (if imported then textStrict "import" else empty),
  textStrict $ T.concat $ List.intersperse "." moduleName]
printParseTree (ModuleName { moduleName}) =
  dealWithLongLine [textStrict "module", textStrict $ T.concat $ List.intersperse "." moduleName , textStrict "where"]

--TODO: The rest of the comments does not get printed because right now it is guaranteed to be 0.
printSignature :: TypeSignature -> Doc
printSignature (TypeSignature {funcName, funcType, comments = (x:y:rest)}) =
  sep $ sep ([textStrict funcName] ++ map printComment x ++ [textStrict":"] ++ map printComment y) : printType funcType False

printExpr :: Expr -> Doc
printExpr expr =
  case expr of
    Ident {name} -> textStrict name
    NumLit {value} -> integer value
    Hole {textInside} -> enclose (textStrict "{!") (textStrict "!}") $ textStrict textInside
    FunctionApp {function, argument} -> addBrackets function </> addBrackets argument

addBrackets :: Expr -> Doc
addBrackets (FunctionApp {function , argument}) =
            parens $ printExpr function </> printExpr argument
addBrackets anything = printExpr anything

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
      where temp = ((sep $ printType input True) </> arrow) : (printType output False)

printPragma :: Pragma -> Doc
printPragma (Builtin {concept, definition})=
  enclose (textStrict "{-# ") (textStrict " #-}") $ sep $
  [textStrict "BUILTIN", textStrict concept, printExpr definition]

printComment :: Comment -> Doc
printComment (LineComment {content}) = textStrict $ append "--" content
printComment (MultiLineComment {content}) = enclose (textStrict"{-") (textStrict"-}") $ textStrict content
