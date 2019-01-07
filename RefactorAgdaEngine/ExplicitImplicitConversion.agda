module ExplicitImplicitConversion where
open import ExpressionChanging
open import Data.Nat
open import Data.List
open import ParseTree
open import ScopeState
open import Data.Vec using (Vec ; _∷_)
open import Data.Fin
open import Data.List.NonEmpty
open import AgdaHelperFunctions
open import Data.Bool
open import ParseTreeOperations

-- is given types before conversion
-- namedArgument for type, never just implicit!!!!!!
conversionIns : {n : ℕ} -> Vec Expr n -> Fin n -> (List Expr -> List Expr)
conversionIns Vec.[] () l
conversionIns _ _ [] = []
conversionIns (namedArgument _ {false} ∷ x₁) zero (implicit y ∷ list) = y ∷ list
-- expecting implicit and getting explicit, i.e. argument to be moved
-- isn't specified.
conversionIns (namedArgument _ {false} ∷ x₁) zero (y ∷ list) =  newUnderscore ∷ y ∷ list
conversionIns (x ∷ x₁) zero (x₂ ∷ list) = implicit x₂ ∷ list
conversionIns (namedArgument _ {false} ∷ x₁) (suc n) (implicit x₂ ∷ list) =
  implicit x₂ ∷ conversionIns x₁ n list
conversionIns (namedArgument _ {false} ∷ x₁) (suc n) (x₂ ∷ list) =
  let result = conversionIns x₁ n (x₂ ∷ list)
  in if headImplicit? result then (implicit newUnderscore) ∷ result
        else result
    where headImplicit? : List Expr -> Bool
          headImplicit? (implicit x ∷ l) = true
          headImplicit? _ = false
conversionIns (x ∷ x₁) (suc n) (x₂ ∷ list) = x₂ ∷ conversionIns x₁ n list

convertInExpr : TypeSignature -> ℕ -> ScopeState (List⁺ Expr -> List⁺ Expr)
convertInExpr t n = makeInstruction t n conversionIns

convertInSignature : {n : ℕ} -> Vec Expr (suc n)  -> Fin n -> ScopeState (Vec Expr (suc n))
convertInSignature (namedArgument arg {isExplicit} {c}{c2} ∷ l) zero =
  return $ namedArgument arg {not isExplicit}{c}{c2} ∷ l
convertInSignature (implicit x ∷ l) zero = return $ x ∷ l
convertInSignature (x ∷ l) zero = do
    newName <- getUniqueIdentifier
    return $ namedArgument (typeSignature newName x) {false} {[]} {[]} ∷ l
convertInSignature (x ∷ v) (suc f) = do
    rest <- convertInSignature v f
    return $ x ∷ rest

convert : List ParseTree -> (funcID : ℕ) -> (whichArgument : ℕ) -> ScopeState (List ParseTree)
convert code funcId whichArgument = doForArgument code funcId whichArgument convertInSignature convertInExpr
