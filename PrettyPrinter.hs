{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyPrinter (prettyPrint) where
import Data.Text (Text, append, pack, intercalate)
import qualified Data.Text as T
import qualified Data.List as List
import ParseTree

prettyPrint :: [ParseTree] -> Text -> Text
prettyPrint newTree oldCode = foldr replaceSection oldCode newTree

replaceSection :: ParseTree -> Text -> Text
replaceSection section inText =
  let r = range section
      before = T.take (fromInteger $ lastUnaffected r) inText
      after = T.drop (fromInteger $ lastAffected r) inText
  in if (needsUpdating r)
     then (T.concat [before, (printParseTree section), after])
     else inText

makeLine :: [Text] -> Text
makeLine x = intercalate " " $ x ++ ["\n"]

printParseTree :: ParseTree -> Text
printParseTree (Signature {signature}) = makeLine $ printSignature signature
printParseTree (FunctionDefinition {definitionOf, params, body}) =
  makeLine $ (definitionOf : (concat $ map addBrackets params)) ++ ("=" : printExpr body)
printParseTree (DataStructure {dataName, parameters, indexInfo, constructors}) =
  makeLine $ ("data" : dataName : (concat $ map signatureInBrackets parameters)) ++
  [":"] ++
  (printType indexInfo False) ++

  ["where\n"] ++
  (map (makeLine . (:) "    " . printSignature) constructors)
printParseTree (Pragma {pragma}) = makeLine $ printPragma pragma
printParseTree (OpenImport { opened, imported, moduleName}) =
  makeLine $ filter (\x -> x /="")
  [(if opened then "open" else ""), (if imported then "import" else ""),
  T.concat $ List.intersperse "." moduleName]
printParseTree (ModuleName { moduleName}) =
  makeLine $ ["module", T.concat $ List.intersperse "." moduleName , "where"]

signatureInBrackets :: TypeSignature -> [Text]
signatureInBrackets x = ("(" : (printSignature x)) ++ [")"]

--TODO: The rest of the comments does not get printed because right now it is guaranteed to be 0.
printSignature :: TypeSignature -> [Text]
printSignature (TypeSignature {funcName, funcType, comments = (x:y:rest)}) =
  (funcName : map printComment x) ++ [":" ] ++ (map printComment y) ++
   (printType funcType False)

printExpr :: Expr -> [Text]
printExpr expr =
  case expr of
    Ident {name} -> [name]
    NumLit {value} -> [pack $ show value]
    Hole {textInside} -> [T.concat ["{!", textInside, "!}"]]
    FunctionApp {function, argument} -> addBrackets function ++ addBrackets argument

addBrackets :: Expr -> [Text]
addBrackets (FunctionApp {function , argument}) =
            ("(" : printExpr function) ++ printExpr argument ++ [")"]
addBrackets anything = printExpr anything

printType :: Type -> Bool -> [Text]
printType t wantBrackets =
  case t of
    Type {expression} -> printExpr expression
    ImplicitArgument { impArg} ->
      ("{" : printSignature impArg) ++ ["}"]
    ExplicitArgument { expArg} ->
      ("(" : printSignature expArg) ++ [")"]
    FunctionType { input, output } ->
      if wantBrackets
      then ["("] ++ (printType input True) ++ ("->" : printType output True) ++ [")"]
      else (printType input True) ++ ("->" : printType output True)

printPragma :: Pragma -> [Text]
printPragma (Builtin {concept, definition})=
  ["{-#", "BUILTIN", concept] ++ printExpr definition ++ ["#-}"]

printComment :: Comment -> Text
printComment (LineComment {content}) = append "--" content
printComment (MultiLineComment {content}) = append "{-" $ append content "-}"
