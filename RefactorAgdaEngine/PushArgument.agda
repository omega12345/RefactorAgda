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
open import ExpressionChanging hiding (makeInstruction)
open import ParseTreeOperations
open import Data.Vec hiding (_>>=_)
open import Data.Fin
open import Relation.Nullary
open import Data.Nat.Properties
open import Data.String using (_++_)

data ArgType : Set where
  explicit : ArgType
  present : ArgType
  absent : ArgType
-- if matches is false then an expected implicit argument has been omitted
matches : Expr -> Expr -> ArgType
matches expectedType argument
  with isImplicit expectedType | isImplicit argument
matches expectedType argument | false | false = explicit
--expected explicit but found implicit, actually an error
matches expectedType argument | false | true = explicit
matches expectedType argument | true | false = absent
matches expectedType argument | true | true = present
-- n is the argument to be pushed, so there must be another argument to switch
-- it with
makeIns : {n : ℕ} -> Vec Expr (suc n) -> Fin n -> (List Expr -> List Expr)
makeIns (x ∷ x₁) _ [] = []
makeIns (x ∷ y ∷ list) zero (z ∷ []) with matches x z
makeIns (x ∷ y ∷ list) zero (z ∷ []) | explicit = newHole ∷ []
makeIns (x ∷ y ∷ list) zero (z ∷ []) | present = implicit newHole ∷ []
makeIns (x ∷ y ∷ list) zero (z ∷ []) | absent with matches y z
makeIns (x ∷ y ∷ list) zero (z ∷ []) | absent | explicit = newHole ∷ []
makeIns (x ∷ y ∷ list) zero (z ∷ []) | absent | present = z ∷ []
makeIns (x ∷ y ∷ list) zero (z ∷ []) | absent | absent = z ∷ []
makeIns (a ∷ b ∷ list) zero (y ∷ z ∷ list2) with
  matches a y
makeIns (a ∷ b ∷ list) zero (y ∷ z ∷ list2) | absent = y ∷ z ∷ list2
makeIns (a ∷ b ∷ list) zero (y ∷ z ∷ list2) | _ with matches b z
makeIns (a ∷ b ∷ list) zero (y ∷ z ∷ list2) | _ | explicit = z ∷ y ∷ list2
makeIns (a ∷ b ∷ list) zero (y ∷ z ∷ list2) | _ | present = z ∷ y ∷ list2
makeIns (a ∷ b ∷ list) zero (y ∷ z ∷ list2) | _ | absent = y ∷ z ∷ list2
makeIns (x ∷ x₁) (suc n) (y ∷ list) with matches x y
makeIns (x ∷ x₁) (suc n) (y ∷ list) | explicit = y ∷ makeIns x₁ n list
makeIns (x ∷ x₁) (suc n) (y ∷ list) | present = y ∷ makeIns x₁ n list
makeIns (x ∷ x₁) (suc n) (y ∷ list) | absent = makeIns x₁ n (y ∷ list)


makeInstruction : TypeSignature -> ℕ -> (makeIns : {n : ℕ} -> Vec Expr (suc n) -> Fin n -> (List Expr -> List Expr)) -> ScopeState (List⁺ Expr -> List⁺ Expr)
makeInstruction (typeSignature funcName funcType) argNo f
  with toVec (typeToList funcType)
... | c = do
    newFin <- makeFin2 c argNo
    return $ function $ f c newFin
    where function : (List Expr -> List Expr) -> (List⁺ Expr -> List⁺ Expr)
          function f (ident identifier₁ ∷ tail₁)
              with sameId identifier₁ funcName
          function f (ident identifier₁ ∷ tail₁) | false =
            (ident identifier₁ ∷ tail₁)
          function f (ident identifier₁ ∷ tail₁) | true =
              ident identifier₁ ∷ f tail₁
          function f list = list

pushInstruction : TypeSignature -> ℕ -> ScopeState (List⁺ Expr -> List⁺ Expr)
pushInstruction t n = makeInstruction t n makeIns

--Data.List.NonEmpty.fromVec
pushTypeIfApplicable : {n : ℕ} -> Vec Expr (suc n)  -> Fin n -> ScopeState (Vec Expr (suc n))
pushTypeIfApplicable (namedArgument (typeSignature funcName funcType) {b} {bef1} {aft1} ∷ namedArgument (typeSignature funcName₁ funcType₁) {b2} {bef2} {aft2} ∷ v) zero =
        if sameName funcName funcName₁
         then fail "Can't switch two arguments of the same name, to avoid anything changing meaning"
         else if funcName doesNotAppearInExp funcType₁
                then (return $  namedArgument (typeSignature funcName₁ funcType₁) {b2} {bef2} {aft2} ∷ namedArgument (typeSignature funcName funcType) {b} {bef1} {aft1} ∷ v)
                else fail "The next argument depends on the one being pushed"
pushTypeIfApplicable (namedArgument (typeSignature funcName funcType) {b} {bef} {aft} ∷ x ∷ v) zero = if funcName doesNotAppearInExp x
                      then return $  x ∷ namedArgument (typeSignature funcName funcType) {b} {bef} {aft} ∷ v
                      else fail "The next argument depends on the one being pushed"
pushTypeIfApplicable (x ∷ x₁ ∷ v) zero = return $  x₁ ∷ x ∷ v
pushTypeIfApplicable (x ∷ v) (suc f) = do
        newRest <- pushTypeIfApplicable v f
        return $ x ∷ newRest

pushArgument : List ParseTree -> (funcID : ℕ) -> (whichArgument : ℕ) -> ScopeState (List ParseTree)
pushArgument code funcId whichArgument = doForArgument code funcId whichArgument pushTypeIfApplicable pushInstruction
