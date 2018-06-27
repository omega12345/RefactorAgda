-- This module is responsible for adding the missing scope information to the parse tree.

module ScopeParseTree where
open import ParseTree
open import Data.Empty
open import ScopeEnv
open import Data.List hiding (_++_)
open import Data.Product hiding (map)
open import Data.Fin
open import Data.Nat
open import Data.Sum
open import Data.String
open import Data.Bool

scopeLiteral : IdentOrLiteral -> ScopeState IdentOrLiteral
scopeLiteral (ident identifier₁) = do
-- first try to find identifier among already declared (simplification for constructor), then assume new declaration (simplification for parameter or out-of-file declaration)
  inj₂ r <- fillInIdentifier identifier₁
   where _ ->  addIdentifier identifier₁ >>= reassemble ident
  return (inj₂ (ident r))
scopeLiteral x = return (inj₂ x)

scopeParameter : Parameter -> ScopeState Parameter
scopeParameter (lit literal) = scopeLiteral literal >>= reassemble lit
scopeParameter (paramApp function argument) = do
  r1 <- scopeParameter function
  r2 <- scopeParameter argument
  reassemble2 paramApp r1 r2

scopeExpr : Expr -> ScopeState Expr
scopeExpr (exprLit literal) = scopeLiteral literal >>= reassemble exprLit
scopeExpr (functionApp e e₁) = do
  r1 <- scopeExpr e
  r2 <- scopeExpr e₁
  reassemble2 functionApp r1 r2
scopeExpr x = return (inj₂ x)

scopeSignature : TypeSignature -> ScopeType -> ScopeState TypeSignature

scopeType : Type -> ScopeState Type
-- simple types do not add either scopes or variables
scopeType (type expression) = scopeExpr expression >>= reassemble type
-- implicit and explicit arguments open a new scope, which is done by
-- scopeSignature
scopeType (implicitArgument impArg) =
  scopeSignature impArg addVariableToType
  >>= reassemble implicitArgument
scopeType (explicitArgument expArg) =
  scopeSignature expArg addVariableToType
  >>= reassemble explicitArgument
scopeType (functionType t t₁) =  do
  result1 <- scopeType t
  result2 <- (scopeType t₁)
  reassemble2 functionType result1 result2


scopeSignature (typeSignature funcName funcType comments) scopeT = do
    rememberScope -- effectively scope in which previous function is declared
    newType <- scopeType funcType -- types are scoped in the context of the previous
    -- scope, the function being scoped is not visible here!
    -- go back up the scope tree, in case subscopes were added for variable types
    returnToRememberedScope
    addScope scopeT
    newId <- addIdentifier funcName
    reassemble2 (λ x y -> typeSignature x y comments) newId newType

scopePragma : Pragma -> ScopeState Pragma
scopePragma (builtin concept definition) =
  fillInIdentifier definition >>= reassemble (builtin concept)

scopeParseTree : ParseTree -> ScopeState ParseTree
scopeParseTree (signature signature₁ range₁) =
  scopeSignature signature₁ addFuncToModule
  >>= reassemble (λ x -> signature x range₁)
scopeParseTree (functionDefinition definitionOf params body range₁) = do
  inj₂ newId <- fillInIdentifier definitionOf
    where inj₁ x -> return (inj₁ x)
  rememberScope
  addScope funcDef
  newParams <- mapState scopeParameter params
  newBody <- scopeExpr body
  returnToRememberedScope
  reassemble2 (λ a b -> functionDefinition newId a b range₁) newParams newBody
scopeParseTree
  (dataStructure dataName parameters indexInfo constructors range₁) = do
    addScope (moduleDef dataName)
    inj₂ newDataName <- addIdentifier dataName
      where inj₁ x -> return (inj₁ x)
    rememberScope
    newParams <- mapState (λ x -> scopeSignature x addVariableToType) parameters
    newIndex <- scopeType indexInfo
    inj₂ newCons <-
        mapState (λ x -> scopeSignature x addFuncToModule) constructors
      where inj₁ x -> return (inj₁ x)
    returnToRememberedScope
    inj₂ r <- mapState addContentReferenceToModuleTop newCons
      where inj₁ x -> return (inj₁ x)
    reassemble2 (λ x y -> dataStructure newDataName x y newCons range₁) newParams newIndex
scopeParseTree (moduleName moduleName₁ range₁) = do
  addScope (moduleDef moduleName₁)
  return (inj₂ (moduleName moduleName₁ range₁))
scopeParseTree (pragma pragma₁ range₁) = do
  scopePragma pragma₁ >>= reassemble (λ x -> pragma x range₁)
scopeParseTree x = return (inj₂ x)

scopeParseTreeList : List ParseTree -> ScopeState (List ParseTree) --Σ (String ⊎ List ParseTree) (λ _ → ScopeEnv)
scopeParseTreeList program = do
  --TODO: fix sloppy workaround
  addIdentifier (identifier "BlockUpPlaceHolderPlace" (λ _ -> false) 0 0)
  addIdentifier (identifier "Set" (λ _ -> false) 0 0)
  mapState scopeParseTree program
