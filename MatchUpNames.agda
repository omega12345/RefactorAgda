module MatchUpNames where
open import ParseTree
open import ScopeState
open import Data.Sum

open import Data.List hiding (lookup)
open import AgdaHelperFunctions

matchUpExpr : Expr -> ScopeState Expr
matchUpExpr (ident identifier₁) = do
  x <- access identifier₁
  return $ ident x
matchUpExpr (functionApp x x₁) = do
  r1 <- matchUpExpr x
  r2 <- matchUpExpr x₁
  return $ functionApp r1 r2
matchUpExpr x =  return x

matchUpSignature : TypeSignature -> ScopeState TypeSignature

matchUpType : Type -> ScopeState Type
matchUpType (type expression) = do
  x <- matchUpExpr expression
  return $ type x
matchUpType (namedArgument arg {b}) = do
  x <- matchUpSignature arg
  return $ namedArgument x {b}
matchUpType (functionType t t₁) =  do
  result1 <- matchUpType t
  result2 <- (matchUpType t₁)
  return $ functionType result1 result2


matchUpSignature (typeSignature funcName funcType) = do
  newName <- access funcName
  newType <- matchUpType funcType
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
  newIndex <- matchUpType indexInfo
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
