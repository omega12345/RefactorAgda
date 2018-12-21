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

scopeSignature : TypeSignature -> ScopeType -> ScopeState TypeSignature

scopeExpr : Expr -> ScopeState Expr
scopeExpr (ident identifier₁) = do
  -- first try to find identifier among already declared (simplification for constructor), then assume new declaration (simplification for parameter or out-of-file declaration)
  inj₂ r <- try $ fillInIdentifier identifier₁
   where _ ->  do x <- addIdentifier identifier₁
                  return $ ident x
  return $ ident r
scopeExpr (functionApp e e₁ {b} ) = do
  r1 <- scopeExpr e
  r2 <- scopeExpr e₁
  return $ functionApp r1 r2 {b}
scopeExpr (namedArgument arg {b} {bef} {aft}) = do
  x <- scopeSignature arg addVariableToType
  return $ namedArgument x {b}{bef}{aft}
scopeExpr x = return x

scopeSignature (typeSignature funcName funcType) scopeT = do
    newType <- saveAndReturnToScope $ scopeExpr funcType
    addScope scopeT
    newId <- addIdentifier funcName
    return $ typeSignature newId newType

scopePragma : Pragma -> ScopeState Pragma
scopePragma (builtin concept definition) = do
  x <- fillInIdentifier definition
  return $ builtin concept x
scopePragma x = return x

scopeParseTree : ParseTree -> ScopeState ParseTree
scopeParseTree (signature signature₁ range₁) = do
  x <- scopeSignature signature₁ addFuncToModule
  return $ signature x range₁
scopeParseTree (functionDefinition definitionOf params body range₁) = do
  newId <- fillInIdentifier definitionOf
  x <- saveAndReturnToScope $ do
          addScope funcDef
          newParams <- mapState scopeExpr params
          newBody <- scopeExpr body
          return $ functionDefinition newId newParams newBody range₁
  return x
scopeParseTree
  (dataStructure dataName parameters indexInfo constructors range₁ {comments}) = do
    addScope $ moduleDef dataName
    newDataName <- addIdentifier dataName
    ( newParams , newIndex) , newCons <- saveAndReturnToScope $ do
              newParams <- mapState (λ x -> scopeSignature x addVariableToType) parameters
              newIndex <- scopeExpr indexInfo
              newCons <- mapState (λ x -> scopeSignature x addFuncToModule) constructors
              return ((newParams , newIndex) , newCons)
    r <- mapState addContentReferenceToModuleTop newCons
    return $ dataStructure newDataName newParams newIndex newCons range₁ {comments}
scopeParseTree (moduleName moduleName₁ range₁) = do
  newMod <- addIdentifier moduleName₁
  addScope $ moduleDef moduleName₁
  return $ moduleName newMod range₁
scopeParseTree (pragma pragma₁ range₁) = do
  x <- scopePragma pragma₁
  return $ pragma x range₁
scopeParseTree x = return x

scopeParseTreeList : List ParseTree -> ScopeState (List ParseTree)
scopeParseTreeList program = do
  --TODO: fix sloppy workaround
  put newEnv
  addIdentifier (identifier "Set" (λ _ -> before) 0 0 {true}{[]} {[]})
  mapState scopeParseTree program
