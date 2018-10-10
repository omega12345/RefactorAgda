module PushArgument where
open import ScopeState
open import Data.List
open import ParseTree
open import Data.Nat
open import AgdaHelperFunctions
open import Data.Unit
open import Data.String
open import Data.Bool
open import Data.Product
open import Data.Maybe

data UsageData : Set where
    usageData : (name : Identifier) -> (explicitOrNot : List Bool) -> (whichArg : ℕ) -> UsageData

isExplicit : Type -> List Bool
isExplicit (functionType (namedArgument arg {explicit}) t₁) = explicit ∷ isExplicit t₁
isExplicit (functionType _ t₁) = true ∷ isExplicit t₁
isExplicit _ = []

pushUsagesSignature : UsageData -> TypeSignature -> ScopeState TypeSignature

-- unlike types, expressions and parameters are left-associative
pushExpr : UsageData -> Expr -> ScopeState Expr

switchLeftAssoc : Expr -> Expr -> ScopeState Expr
switchLeftAssoc (functionApp e1 e3) e2 = return $ functionApp (functionApp e1 e2) e3
switchLeftAssoc _ _ = fail "Trying to push an argument which does not exist"

pushLeftAssoc : UsageData -> Expr -> ScopeState (Expr × Maybe ℕ)
pushLeftAssoc (usageData name explicitOrNot whichArg) (ident identifier₁) with sameId name identifier₁
pushLeftAssoc (usageData name explicitOrNot whichArg) (ident identifier₁) | false = return $ (ident identifier₁) , nothing
pushLeftAssoc (usageData name explicitOrNot whichArg) (ident identifier₁) | true = return $ (ident identifier₁) , just (suc whichArg)
pushLeftAssoc ud (functionApp e e₁) = do
    newResult <- pushExpr ud e₁
    e2 , just zero <- pushLeftAssoc ud e
      where e2 , nothing -> return ( (functionApp e2 newResult) , nothing )
            e2 , just (suc n) -> return $ (functionApp e2 newResult) , just n
    newExpr <- switchLeftAssoc e2 newResult
    return (newExpr , nothing)

pushLeftAssoc ud x = return $ x , nothing

pushExpr ud e = do
  result , number <- pushLeftAssoc ud e
  return result

pushType : UsageData -> Type -> ScopeState Type
pushType ud (type expression) = do
  newExpr <- pushExpr ud expression
  return $ type newExpr
pushType ud (namedArgument arg {explicit}) = do
  newArg <- pushUsagesSignature ud arg
  return $ namedArgument newArg {explicit}
pushType ud (functionType t t₁) = do
  newT1 <- pushType ud t
  newT2 <- pushType ud t₁
  return $ functionType newT1 newT2

pushUsagesSignature ud (typeSignature funcName funcType) = do
  newType <- pushType ud funcType
  return $ typeSignature funcName newType
{-}
--TODO This needs to be changed when adding full implicit argument support
makeParameter : Bool -> Identifier -> Parameter
makeParameter b id = lit $ ident id
-}
pushListItem : List Expr -> ℕ -> List Bool -> ScopeState (List Expr)
pushListItem [] zero b = return []
pushListItem [] (suc num) b = return []
pushListItem (x ∷ []) zero (b ∷ bs) = do
  newId <- getUniqueIdentifier
  return $ x ∷ ident newId ∷ []
pushListItem (x ∷ []) zero [] = fail "Trying to push the last argument"
pushListItem (x ∷ x₁ ∷ list) zero b = return $ x₁ ∷ x ∷ list
pushListItem (x ∷ list) (suc num) b = do
  newList <- pushListItem list num b
  return $ x ∷ newList

pushUsages : UsageData -> ParseTree -> ScopeState ParseTree
pushUsages ud (signature signature₁ range₁) = do
    s <- pushUsagesSignature ud signature₁
    return $ signature s range₁
pushUsages (usageData name t whichArg) (functionDefinition definitionOf params body range₁) with sameId name definitionOf
pushUsages (usageData name t whichArg) (functionDefinition definitionOf params body range₁) | false = do
  newParams <- mapState (pushExpr (usageData name t whichArg)) params
  newBody <- pushExpr (usageData name t whichArg) body
  return $ functionDefinition definitionOf newParams newBody range₁
pushUsages (usageData name t whichArg) (functionDefinition definitionOf params body range₁) | true = do
  newParams <- pushListItem params whichArg t
  newBody <- pushExpr (usageData name t whichArg) body
  return $ functionDefinition definitionOf newParams newBody range₁
pushUsages ud (dataStructure dataName parameters indexInfo constructors range₁ {comments}) = do
  newParams <- mapState (pushUsagesSignature ud) parameters
  newIndex <- pushType ud indexInfo
  newCons <- mapState (pushUsagesSignature ud) constructors
  return $ dataStructure dataName newParams newIndex newCons range₁ {comments}
pushUsages _ code = return code

_doesNotAppearInExp_ : Identifier -> Expr -> ScopeState ⊤
x doesNotAppearInExp numLit = return tt
identifier name₁ isInRange₁ scope₁ declaration₁ doesNotAppearInExp ident (identifier name isInRange scope declaration) with compare declaration₁ declaration
(identifier name₁ isInRange₁ scope₁ declaration₁) doesNotAppearInExp (ident (identifier name isInRange scope .declaration₁)) | equal .declaration₁ = fail "Argument to be pushed is used in the type of the next argument"
... | _ = return tt
x doesNotAppearInExp hole = return tt
x doesNotAppearInExp functionApp y y₁ = do
    x doesNotAppearInExp y
    x doesNotAppearInExp y₁

_doesNotAppearIn_ : Identifier -> Type -> ScopeState ⊤
_doesNotAppearInArg_ : Identifier -> TypeSignature -> ScopeState ⊤
x doesNotAppearInArg typeSignature funcName funcType = x doesNotAppearIn funcType


x doesNotAppearIn type expression = x doesNotAppearInExp expression
x doesNotAppearIn namedArgument arg = x doesNotAppearInArg arg
x doesNotAppearIn functionType y y₁ = do
  x doesNotAppearIn y
  x doesNotAppearIn y₁

-- funny symbol is  \ top
_isNotUsedIn_ : Type -> Type -> ScopeState ⊤
namedArgument (typeSignature funcName funcType) isNotUsedIn y = funcName doesNotAppearIn y
x isNotUsedIn y = return tt

_hasDifferentNameFrom_ : Type -> Type -> ScopeState ⊤
namedArgument (typeSignature (identifier name isInRange scope declaration) funcType) hasDifferentNameFrom namedArgument (typeSignature (identifier name₁ isInRange₁ scope₁ declaration₁) funcType₁) with name == name₁
... | true = fail "Arguments to be switched have the same name."
... | false = return tt
x hasDifferentNameFrom y = return tt

-- TODO : Need to move comments also
makeSwitch : Type -> Type -> ScopeState Type
makeSwitch toBePushed (functionType result result₁) = do
  toBePushed isNotUsedIn result
  toBePushed hasDifferentNameFrom result
  return $ functionType result $ functionType toBePushed result₁
makeSwitch toBePushed _ = fail "Selected argument is already the last."

pushArg : Type -> ℕ -> ScopeState Type
pushArg (functionType t t₁) zero = makeSwitch t t₁
pushArg (functionType t t₁) (suc n) = do
  newResult <- pushArg t₁ n
  return $ functionType t newResult
pushArg singleType n = fail "Can't push the result to the right"

pushArgument : List ParseTree -> ℕ -> ℕ -> ScopeState (List ParseTree)
pushArgument [] funcID whichArgument = fail "funcId not found in pushArgument"
pushArgument (signature (typeSignature (identifier name isInRange scope declaration) funcType) range₁ ∷ code) funcID whichArgument with compare declaration funcID
pushArgument (signature (typeSignature (identifier name isInRange scope declaration {b} {a}) funcType) range₁ ∷ code) .declaration whichArgument | equal .declaration = do
    newType <- pushArg funcType whichArgument
    newCode <- mapState ( pushUsages $ usageData (identifier name isInRange scope declaration {b} {a}) (isExplicit funcType) whichArgument) code
    return $ (signature (typeSignature (identifier name isInRange scope declaration {b} {a}) newType) range₁ ∷ newCode)
pushArgument (signature (typeSignature (identifier name isInRange scope declaration {b} {a}) funcType) range₁ ∷ code) funcID whichArgument | _ = do
  newxs <- pushArgument code funcID whichArgument
  return $ (signature (typeSignature (identifier name isInRange scope declaration {b} {a}) funcType) range₁) ∷ newxs
pushArgument (x ∷ xs) funcID whichArgument = do
  newxs <- pushArgument xs funcID whichArgument
  return $ x ∷ newxs
