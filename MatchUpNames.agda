module MatchUpNames where
open import ParseTree
open import ScopeState
open import Data.Sum

open import Data.List hiding (lookup)
open import AgdaHelperFunctions

matchUpIdentOrLit : IdentOrLiteral -> ScopeState IdentOrLiteral
matchUpIdentOrLit (ident identifier₁) = do
  x <- access identifier₁
  return $ ident x
matchUpIdentOrLit x =  return x

matchUpExpr : Expr -> ScopeState Expr
matchUpExpr (exprLit literal) = do
  x <- matchUpIdentOrLit literal
  return $ exprLit x
matchUpExpr (functionApp x x₁) = do
  r1 <- matchUpExpr x
  r2 <- matchUpExpr x₁
  return $ functionApp r1 r2
matchUpExpr x =  return x

matchUpParameter : Parameter -> ScopeState Parameter
matchUpParameter (lit literal) = do
  x <- matchUpIdentOrLit literal
  return $ lit x
matchUpParameter (paramApp x x₁) = do
  r1 <- matchUpParameter x
  r2 <- matchUpParameter x₁
  return $ paramApp r1 r2

matchUpSignature : TypeSignature -> ScopeState TypeSignature

matchUpType : Type -> ScopeState Type
matchUpType (type expression) = do
  x <- matchUpExpr expression
  return $ type x
matchUpType (implicitArgument impArg) = do
  x <- matchUpSignature impArg
  return $ implicitArgument x
matchUpType (explicitArgument expArg) = do
  x <- matchUpSignature expArg
  return $ explicitArgument x
matchUpType (functionType t t₁) =  do
  result1 <- matchUpType t
  result2 <- (matchUpType t₁)
  return $ functionType result1 result2


matchUpSignature (typeSignature funcName funcType comments) = do
  newName <- access funcName
  newType <- matchUpType funcType
  return $ typeSignature newName newType comments

matchUpPragma : Pragma -> ScopeState Pragma
matchUpPragma (builtin concept definition) = do
  x <- access definition
  return $ builtin concept x

matchUpTree : ParseTree -> ScopeState ParseTree
matchUpTree (signature signature₁ range₁) = do
  x <- matchUpSignature signature₁
  return $ signature x range₁
matchUpTree (functionDefinition definitionOf params body range₁) = do
  newDef <- access definitionOf
  newParams <- mapState matchUpParameter params
  newBody <- matchUpExpr body
  return $ functionDefinition newDef newParams newBody range₁
matchUpTree (dataStructure dataName parameters indexInfo constructors range₁) = do
  newName <- access dataName
  newParams <- mapState matchUpSignature parameters
  newIndex <- matchUpType indexInfo
  newCons <- mapState matchUpSignature constructors
  return $ dataStructure newName newParams newIndex newCons range₁
matchUpTree (pragma pragma₁ range₁) = do
  x <- matchUpPragma pragma₁
  return $ pragma x range₁
matchUpTree x = return x

matchUpNames : List ParseTree -> ScopeState (List ParseTree)
matchUpNames list = mapState matchUpTree list
