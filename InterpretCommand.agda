module InterpretCommand where

open import ParseTree
open import Data.List
open import Data.Nat
open import ScopeEnv
open import Data.Sum hiding (map)
open import Data.Bool
open import Data.String hiding (_++_)
open import Data.Maybe hiding (map)


getRange : ParseTree -> Range
getRange (signature _ range₁) = range₁
getRange (functionDefinition _ _ _ range₁) = range₁
getRange (dataStructure _ _ _ _ range₁) = range₁
getRange (pragma _ range₁) = range₁
getRange (openImport _ _ _ range₁) = range₁
getRange (moduleName _ range₁) = range₁

_<=_ : ℕ -> ℕ -> Bool
zero <= zero = true
zero <= suc m = true
suc n <= zero = false
suc n <= suc m = n <= m

isInRange : Range -> ℕ -> Bool
isInRange (range lastUnaffected lastAffected) what = ((lastUnaffected + 1) <= what) ∧ (what <= (lastAffected + 1))

firstActualValue : {A : Set} -> List (Maybe A) -> Maybe A
firstActualValue [] = nothing
firstActualValue (just x ∷ list) = just x
firstActualValue (nothing ∷ list) = firstActualValue list

findInIdentifier : ℕ -> Identifier -> Maybe ℕ
findInIdentifier n (identifier name isInRange₁ scope declaration) =
  if (isInRange₁ n) then (just declaration) else nothing

findInIdentOrLiteral : ℕ -> IdentOrLiteral -> Maybe ℕ
findInIdentOrLiteral n (numLit value) = nothing
findInIdentOrLiteral n (ident identifier₁) = findInIdentifier n identifier₁

findInExpr : ℕ -> Expr -> Maybe ℕ
findInExpr n (exprLit literal) = findInIdentOrLiteral n literal
findInExpr n (hole textInside) = nothing
findInExpr n (functionApp e e₁) = firstActualValue (findInExpr n e ∷ findInExpr n e₁ ∷ [])

findInParam : ℕ -> Parameter -> Maybe ℕ
findInParam n (lit paramLit) = findInIdentOrLiteral n paramLit
findInParam n (paramApp p p₁) = firstActualValue (findInParam n p ∷ findInParam n p₁ ∷ [])

findInType : ℕ -> Type -> Maybe ℕ

findInSignature : ℕ -> TypeSignature -> Maybe ℕ
findInSignature n (typeSignature funcName funcType comments) =
  firstActualValue (findInIdentifier n funcName ∷ findInType n funcType ∷ [])


findInType n (type expression) = findInExpr n expression
findInType n (implicitArgument impArg) = findInSignature n impArg
findInType n (explicitArgument expArg) = findInSignature n expArg
findInType n (functionType t t₁) = firstActualValue (findInType n t ∷ findInType n t₁ ∷ [])

findInParseTree : ℕ -> ParseTree -> Maybe ℕ
findInParseTree n (signature signature₁ range₁) = findInSignature n signature₁
findInParseTree n (functionDefinition definitionOf params body range₁) =
  firstActualValue (findInIdentifier n definitionOf ∷ findInExpr n body ∷ map (findInParam n) params)
findInParseTree n (dataStructure dataName parameters indexInfo constructors range₁) = firstActualValue ((findInIdentifier n dataName ∷ findInType n indexInfo ∷ map (findInSignature n) parameters) ++ map (findInSignature n) constructors)
findInParseTree n (pragma (builtin concept definition) range₁) =
  findInIdentifier n definition
findInParseTree n (openImport opened imported moduleName₁ range₁) = nothing
findInParseTree n (moduleName moduleName₁ range₁) = nothing

getDeclarationIDForPoint : List ParseTree -> ℕ -> Maybe ℕ
getDeclarationIDForPoint [] point = nothing
getDeclarationIDForPoint (x ∷ list) point with isInRange (getRange x) point
getDeclarationIDForPoint (x ∷ list) point | false =
  getDeclarationIDForPoint list point
getDeclarationIDForPoint (x ∷ list) point | true = findInParseTree point x
