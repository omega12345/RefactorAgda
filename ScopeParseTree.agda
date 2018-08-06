-- This module is responsible for adding the missing scope information to the parse tree.

module ScopeParseTree where
open import ParseTree
open import Data.Empty
open import ScopeState
open import Data.List hiding (_++_)
open import Data.Product hiding (map)
open import Data.Fin
open import Data.Nat
open import Data.Sum
open import Data.String
open import Data.Bool
open import AgdaHelperFunctions

scopeLiteral : IdentOrLiteral -> ScopeState IdentOrLiteral
scopeLiteral (ident identifier₁) = do
-- first try to find identifier among already declared (simplification for constructor), then assume new declaration (simplification for parameter or out-of-file declaration)
  inj₂ r <- try $ fillInIdentifier identifier₁
   where _ ->  do x <- addIdentifier identifier₁
                  return $ ident x
  return $ ident r
scopeLiteral x = return x

scopeParameter : Parameter -> ScopeState Parameter
scopeParameter (lit literal) = do x <- scopeLiteral literal
                                  return $ lit x
scopeParameter (paramApp function argument) = do
  r1 <- scopeParameter function
  r2 <- scopeParameter argument
  return $ paramApp r1 r2

scopeExpr : Expr -> ScopeState Expr
scopeExpr (exprLit literal) = do
  x <- scopeLiteral literal
  return $ exprLit x
scopeExpr (functionApp e e₁) = do
  r1 <- scopeExpr e
  r2 <- scopeExpr e₁
  return $ functionApp r1 r2
scopeExpr x = return x

scopeSignature : TypeSignature -> ScopeType -> ScopeState TypeSignature

scopeType : Type -> ScopeState Type
-- simple types do not add either scopes or variables
scopeType (type expression) = do
  x <- scopeExpr expression
  return $ type x
-- implicit and explicit arguments open a new scope, which is done by
-- scopeSignature
scopeType (implicitArgument impArg) = do
  x <- scopeSignature impArg addVariableToType
  return $ implicitArgument x
scopeType (explicitArgument expArg) = do
  x <- scopeSignature expArg addVariableToType
  return $ explicitArgument x
scopeType (functionType t t₁) =  do
  result1 <- scopeType t
  result2 <- scopeType t₁
  return $ functionType result1 result2

scopeSignature (typeSignature funcName funcType comments) scopeT = do
    newType <- saveAndReturnToScope $ scopeType funcType
    addScope scopeT
    newId <- addIdentifier funcName
    return $ typeSignature newId newType comments

scopePragma : Pragma -> ScopeState Pragma
scopePragma (builtin concept definition) = do
  x <- fillInIdentifier definition
  return $ builtin concept x

scopeParseTree : ParseTree -> ScopeState ParseTree
scopeParseTree (signature signature₁ range₁) = do
  x <- scopeSignature signature₁ addFuncToModule
  return $ signature x range₁
scopeParseTree (functionDefinition definitionOf params body range₁) = do
  newId <- fillInIdentifier definitionOf
  x <- saveAndReturnToScope $ do
          addScope funcDef
          newParams <- mapState scopeParameter params
          newBody <- scopeExpr body
          return $ functionDefinition newId newParams newBody range₁
  return x
scopeParseTree
  (dataStructure dataName parameters indexInfo constructors range₁) = do
    addScope $ moduleDef dataName
    newDataName <- addIdentifier dataName
    ( newParams , newIndex) , newCons <- saveAndReturnToScope $ do
              newParams <- mapState (λ x -> scopeSignature x addVariableToType) parameters
              newIndex <- scopeType indexInfo
              newCons <- mapState (λ x -> scopeSignature x addFuncToModule) constructors
              return ((newParams , newIndex) , newCons)
    r <- mapState addContentReferenceToModuleTop newCons
    return $ dataStructure newDataName newParams newIndex newCons range₁
scopeParseTree (moduleName moduleName₁ range₁) = do
  addScope $ moduleDef moduleName₁
  return $ moduleName moduleName₁ range₁
scopeParseTree (pragma pragma₁ range₁) = do
  x <- scopePragma pragma₁
  return $ pragma x range₁
scopeParseTree x = return x

scopeParseTreeList : List ParseTree -> ScopeState (List ParseTree)
scopeParseTreeList program = do
  --TODO: fix sloppy workaround
  addIdentifier (identifier "BlockUpPlaceHolderPlace" (λ _ -> false) 0 0)
  addIdentifier (identifier "Set" (λ _ -> false) 0 0)
  mapState scopeParseTree program
