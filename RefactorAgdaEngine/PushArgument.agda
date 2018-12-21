module PushArgument where
open import ScopeState
open import Data.List.NonEmpty
open import Data.List
open import ParseTree
open import Data.Nat
open import AgdaHelperFunctions
open import Data.Unit
open import Data.String hiding (toVec)
open import Data.Bool
open import Data.Product
open import Data.Maybe
open import ExpressionChanging
open import ParseTreeOperations
open import Data.Vec hiding (_>>=_)
open import Data.Fin
open import Relation.Nullary
open import Data.Nat.Properties
open import Data.String using (_++_)
open import Data.Nat.Show

-- n is the argument to be pushed, so there must be another argument to switch
-- it with
makeIns : {n : ℕ} -> Vec Expr (suc n) -> Fin n -> (List Expr -> List Expr)
makeIns (x ∷ x₁) zero [] = []
makeIns (implicit x ∷ implicit x₁ ∷ x₂) zero (implicit y ∷ []) = implicit (underscore {emptyRange} {[]} {[]}) ∷ implicit y ∷ []
makeIns (implicit x ∷ x₁) zero (implicit y ∷ x₂ ∷ list) = x₂ ∷ implicit y ∷ list
-- expecting implicit and getting explicit, i.e. argument to be moved
-- isn't specified.
makeIns (implicit x ∷ x₁) zero (y ∷ list) = y ∷ list
makeIns (x ∷ x₁) zero (x₂ ∷ []) = newHole ∷ []
makeIns (x ∷ x₁) zero (x₂ ∷ x₃ ∷ list) = x₃ ∷ x₂ ∷ list
makeIns (x ∷ x₁) (suc n) [] = []
makeIns (implicit x ∷ x₁) (suc n) (implicit x₂ ∷ list) =
  implicit x₂ ∷ makeIns x₁ n list
makeIns (implicit x ∷ x₁) (suc n) (x₂ ∷ list) = makeIns x₁ n list
makeIns (x ∷ x₁) (suc n) (x₂ ∷ list) = x₂ ∷ makeIns x₁ n list

makeFin : {n : ℕ} -> Vec Expr (suc n) -> ℕ -> ScopeState (Fin n)
makeFin {n} v number with number Data.Nat.Properties.<? n
makeFin {n} v number | yes p = return $ fromℕ≤ p
makeFin {n} v number | no ¬p = fail $ "Got a non-existing argument: " Data.String.++ Data.Nat.Show.show n Data.String.++ Data.Nat.Show.show (Data.List.length $ Data.Vec.toList v)

pushInstruction : TypeSignature -> ℕ -> ScopeState (List⁺ Expr -> List⁺ Expr)
pushInstruction (typeSignature funcName funcType) argNo
  with toVec (typeToList funcType)
... | c = do
    newFin <- makeFin c argNo
    return $ function $ makeIns c newFin
    where function : (List Expr -> List Expr) -> (List⁺ Expr -> List⁺ Expr)
          function f (ident identifier₁ ∷ tail₁)
              with sameId identifier₁ funcName
          function f (ident identifier₁ ∷ tail₁) | false =
            (ident identifier₁ ∷ tail₁)
          function f (ident identifier₁ ∷ tail₁) | true =
              ident identifier₁ ∷ f tail₁
          function f list = list

--TODO: try to give this a better type
updateSignature :  ℕ -> List⁺ Expr -> ScopeState (List⁺ Expr)
updateSignature argNo type
  with toVec type
... | c = do
  newFin <- makeFin c argNo
  newVec <- pushTypeIfApplicable c newFin
  return $ Data.List.NonEmpty.fromVec newVec
  where pushTypeIfApplicable : {n : ℕ} -> Vec Expr (suc n)  -> Fin n -> ScopeState (Vec Expr (suc n) )
        pushTypeIfApplicable (namedArgument (typeSignature funcName funcType) {b} {bef1} {aft1} ∷ namedArgument (typeSignature funcName₁ funcType₁) {b2} {bef2} {aft2} ∷ v) zero =
          if sameName funcName funcName₁
           then fail "Can't switch two arguments of the same name, to avoid anything changing meaning"
           else if funcName doesNotAppearInExp funcType₁
                  then (return $ namedArgument (typeSignature funcName₁ funcType₁) {b2} {bef2} {aft2} ∷ namedArgument (typeSignature funcName funcType) {b} {bef1} {aft1} ∷ v)
                  else fail "The next argument depends on the one being pushed"
        pushTypeIfApplicable (namedArgument (typeSignature funcName funcType) {b} {bef} {aft} ∷ x ∷ v) zero = if funcName doesNotAppearInExp x
                        then return $ x ∷ namedArgument (typeSignature funcName funcType) {b} {bef} {aft} ∷ v
                        else fail "The next argument depends on the one being pushed"
        pushTypeIfApplicable (x ∷ x₁ ∷ v) zero = return $ x₁ ∷ x ∷ v
        pushTypeIfApplicable (x ∷ v) (suc f) = do
          newRest <- pushTypeIfApplicable v f
          return $ x ∷ newRest

{-zero (head₁ ∷ []) = head₁ ∷ []
updateSignature zero (head₁ ∷ x ∷ tail₁) = x ∷ head₁ ∷ tail₁
updateSignature (suc n) (head₁ ∷ []) = head₁ ∷ []
updateSignature (suc n) (head₁ ∷ x ∷ tail₁) = head₁ ∷⁺ updateSignature n (x ∷ tail₁)-}

pushArgument : List ParseTree -> ℕ -> ℕ -> ScopeState (List ParseTree)
pushArgument [] funcID whichArgument = fail "funcId not found in pushArgument"
pushArgument (signature (typeSignature (identifier name isInRange scope declaration) funcType) range₁ ∷ code) funcID whichArgument with Data.Nat.compare declaration funcID
pushArgument (signature (typeSignature (identifier name isInRange scope declaration {e}{b} {a}) funcType) range₁ ∷ code) .declaration whichArgument | equal .declaration = do
    newType <- updateSignature whichArgument (typeToList funcType)
    instruction <- pushInstruction ((typeSignature (identifier name isInRange scope declaration {e}{b} {a})) funcType) whichArgument
    return $ applyToProgram instruction $
      signature (typeSignature
                  (identifier name isInRange scope declaration {e}{b} {a})
                  (listToType newType)) range₁ ∷ code
pushArgument (signature (typeSignature (identifier name isInRange scope declaration {e}{b} {a}) funcType) range₁ ∷ code) funcID whichArgument | _ = do
  newxs <- pushArgument code funcID whichArgument
  return $ (signature (typeSignature (identifier name isInRange scope declaration {e}{b} {a}) funcType) range₁) ∷ newxs
pushArgument (x ∷ xs) funcID whichArgument = do
  newxs <- pushArgument xs funcID whichArgument
  return $ x ∷ newxs
--162,7900
