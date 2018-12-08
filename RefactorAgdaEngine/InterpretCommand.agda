module InterpretCommand where

open import ParseTree
open import Data.List
open import Data.Nat
open import ScopeState
open import Data.Sum hiding (map)
open import Data.Bool
open import Data.String hiding (_++_)
open import Data.Maybe hiding (map)
open import Data.Product hiding (map)
open import AgdaHelperFunctions

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
findInIdentifier n (identifier name isInRange₁ scope declaration)
  with isInRange₁ n
... | inside = just declaration
... | x = nothing

findInSignature : ℕ -> TypeSignature -> Maybe ℕ

findInExpr : ℕ -> Expr -> Maybe ℕ
findInExpr n numLit = nothing
findInExpr n (ident i) = findInIdentifier n i
findInExpr n hole = nothing
findInExpr n (functionApp e e₁) = firstActualValue (findInExpr n e ∷ findInExpr n e₁ ∷ [])
findInExpr n (implicit e) = findInExpr n e
findInExpr n underscore = nothing
findInExpr n (namedArgument arg) = findInSignature n arg


findInSignature n (typeSignature funcName funcType) =
  firstActualValue (findInIdentifier n funcName ∷ findInExpr n funcType ∷ [])

findInParseTree : ℕ -> ParseTree -> Maybe ℕ
findInParseTree n (signature signature₁ range₁) = findInSignature n signature₁
findInParseTree n (functionDefinition definitionOf params body range₁) =
  firstActualValue (findInIdentifier n definitionOf ∷ findInExpr n body ∷ map (findInExpr n) params)
findInParseTree n (dataStructure dataName parameters indexInfo constructors range₁) = firstActualValue ((findInIdentifier n dataName ∷ findInExpr n indexInfo ∷ map (findInSignature n) parameters) ++ map (findInSignature n) constructors)
findInParseTree n (pragma (builtin concept definition) range₁) =
  findInIdentifier n definition
findInParseTree _ _ = nothing

getDeclarationIDForPoint : List ParseTree -> ℕ -> Maybe ℕ
getDeclarationIDForPoint [] point = nothing
getDeclarationIDForPoint (x ∷ list) point with isInRange (getRange x) point
getDeclarationIDForPoint (x ∷ list) point | false =
  getDeclarationIDForPoint list point
getDeclarationIDForPoint (x ∷ list) point | true = findInParseTree point x

-- for argument rearranging

--TODO fix this when adding more detailed source position data
getArgNumber : ℕ -> ℕ -> Expr -> ScopeState ℕ
getArgNumber point currArgNumber (functionApp t t₁ {true}) = do
  just _ <- return $ findInExpr point t
    where _ -> getArgNumber point (currArgNumber + 1) t₁
  return currArgNumber
getArgNumber point currArgNumber t = fail "Point seems to be in result."

getFuncIdAndArgNumberInParseTree : ℕ -> ParseTree -> ScopeState (ℕ × ℕ)
getFuncIdAndArgNumberInParseTree n (signature (typeSignature (identifier name isInRange₁ scope declaration) funcType) range₁) = do
    argNum <- getArgNumber n zero funcType
    return $ declaration , argNum
getFuncIdAndArgNumberInParseTree _ _ = fail "Point not in signature"

getFuncIdAndArgNumber : List ParseTree -> ℕ -> ScopeState (ℕ × ℕ)
getFuncIdAndArgNumber [] point = fail "Point not in code"
getFuncIdAndArgNumber (x ∷ list) point with isInRange (getRange x) point
getFuncIdAndArgNumber (x ∷ list) point | false =
  getFuncIdAndArgNumber list point
getFuncIdAndArgNumber (x ∷ list) point | true = getFuncIdAndArgNumberInParseTree point x
