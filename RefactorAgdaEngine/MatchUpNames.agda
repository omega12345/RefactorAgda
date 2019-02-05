module MatchUpNames where
open import ParseTree
open import ScopeState
open import Data.Sum

open import Data.List hiding (lookup)
open import AgdaHelperFunctions

matchUpSignature : TypeSignature -> ScopeState TypeSignature

matchUpExpr : Expr -> ScopeState Expr
matchUpExpr (ident identifier₁) = do
  x <- access identifier₁
  return $ ident x
matchUpExpr (functionApp x x₁ {b} ) = do
  r1 <- matchUpExpr x
  r2 <- matchUpExpr x₁
  return $ functionApp r1 r2 {b}
matchUpExpr (namedArgument arg {b} {bef} {aft}) = do
  x <- matchUpSignature arg
  return $ namedArgument x {b} {bef} {aft}
matchUpExpr x =  return x




matchUpSignature (typeSignature funcName funcType) = do
  newName <- access funcName
  newType <- matchUpExpr funcType
  return $ typeSignature newName newType

matchUpPragma : Pragma -> ScopeState Pragma
matchUpPragma (builtin concept definition) = do
  x <- access definition
  return $ builtin concept x
matchUpPragma x = return x

matchUpTree : ParseTree -> ScopeState ParseTree
matchUpTree (signature signature₁ range₁) = do
  x <- matchUpSignature signature₁
  return $ signature x range₁
matchUpTree (functionDefinition definitionOf params body range₁) = do
  newDef <- access definitionOf
  newParams <- mapState matchUpExpr params
  newBody <- matchUpExpr body
  return $ functionDefinition newDef newParams newBody range₁
matchUpTree (dataStructure dataName parameters indexInfo constructors range₁ {comments}) = do
  newName <- access dataName
  newParams <- mapState matchUpSignature parameters
  newIndex <- matchUpExpr indexInfo
  newCons <- mapState matchUpSignature constructors
  return $ dataStructure newName newParams newIndex newCons range₁ {comments}
matchUpTree (pragma pragma₁ range₁) = do
  x <- matchUpPragma pragma₁
  return $ pragma x range₁
matchUpTree (moduleName id r) = do
  x <- access id
  return $ moduleName x r
matchUpTree x = return x

matchUpNames : List ParseTree -> ScopeState (List ParseTree)
matchUpNames list = mapState matchUpTree list
