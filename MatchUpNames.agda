module MatchUpNames where
open import ParseTree
open import ScopeEnv
open import Data.Sum

open import Data.List

matchUpIdentOrLit : IdentOrLiteral -> ScopeState IdentOrLiteral
matchUpIdentOrLit (ident identifier₁) =
  renameIdentifier identifier₁ >>= reassemble ident
matchUpIdentOrLit x =  return (inj₂ x)

matchUpExpr : Expr -> ScopeState Expr
matchUpExpr (exprLit literal) =
  matchUpIdentOrLit literal >>= reassemble exprLit
matchUpExpr (functionApp x x₁) = do
  r1 <- matchUpExpr x
  r2 <- matchUpExpr x₁
  reassemble2 functionApp r1 r2
matchUpExpr x =  return (inj₂ x)

matchUpParameter : Parameter -> ScopeState Parameter
matchUpParameter (lit literal) =
  matchUpIdentOrLit literal >>= reassemble lit
matchUpParameter (paramApp x x₁) = do
  r1 <- matchUpParameter x
  r2 <- matchUpParameter x₁
  reassemble2 paramApp r1 r2

matchUpSignature : TypeSignature -> ScopeState TypeSignature

matchUpType : Type -> ScopeState Type
matchUpType (type expression) = matchUpExpr expression >>= reassemble type
matchUpType (implicitArgument impArg) = matchUpSignature impArg >>= reassemble implicitArgument
matchUpType (explicitArgument expArg) = matchUpSignature expArg >>= reassemble explicitArgument
matchUpType (functionType t t₁) =  do
  result1 <- matchUpType t
  result2 <- (matchUpType t₁)
  reassemble2 functionType result1 result2


matchUpSignature (typeSignature funcName funcType comments) = do
  newName <- renameDeclaringIdentifier funcName
  newType <- matchUpType funcType
  reassemble2 (λ x y -> typeSignature x y comments) newName newType

matchUpPragma : Pragma -> ScopeState Pragma
matchUpPragma (builtin concept definition) =
  renameIdentifier definition >>= reassemble (builtin concept)

matchUpTree : ParseTree -> ScopeState ParseTree
matchUpTree (signature signature₁ range₁) =
  matchUpSignature signature₁ >>= reassemble (λ x -> signature x range₁)
matchUpTree (functionDefinition definitionOf params body range₁) = do
  (inj₂ newDef) <- renameDeclaringIdentifier definitionOf
    where (inj₁ x) -> return (inj₁ x)
  newParams <- mapState matchUpParameter params
  newBody <- matchUpExpr body
  reassemble2 (λ x y -> functionDefinition newDef x y range₁) newParams newBody
matchUpTree (dataStructure dataName parameters indexInfo constructors range₁) = do
  (inj₂ newName) <- renameDeclaringIdentifier dataName
    where (inj₁ x) -> return (inj₁ x)
  (inj₂ newParams) <- mapState matchUpSignature parameters
    where (inj₁ x) -> return (inj₁ x)
  newIndex <- matchUpType indexInfo
  newCons <- mapState matchUpSignature constructors
  reassemble2 (λ x y -> dataStructure newName newParams x y range₁) newIndex newCons
matchUpTree (pragma pragma₁ range₁) =
  matchUpPragma pragma₁ >>= reassemble (λ x -> pragma x range₁)
matchUpTree x = return (inj₂ x)

matchUpNames : List ParseTree -> ScopeState (List ParseTree)
matchUpNames list = mapState matchUpTree list
