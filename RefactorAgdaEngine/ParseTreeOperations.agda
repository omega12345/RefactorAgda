module ParseTreeOperations where
open import ParseTree
open import Data.List.NonEmpty
open import Data.Bool
open import Data.List hiding ([_])
open import Data.Nat
open import Relation.Nullary
-- go from ((f a) b) representation to f [a, b]
expressionToList : Expr -> List⁺ Expr
expressionToList (functionApp e e₁ {false}) = expressionToList e ⁺∷ʳ e₁
expressionToList x = [ x ]

typeToList : Expr -> List⁺ Expr
typeToList (functionApp e e₁ {true}) = e ∷⁺ typeToList e₁
typeToList x = [ x ]

-- go from f [a, b] representation to ((f a) b)
{-# TERMINATING #-}
listToExpression : List⁺ Expr -> Expr
listToExpression (head₁ ∷ []) = head₁
listToExpression (head₁ ∷ x ∷ tail₁) =
  listToExpression (functionApp head₁ x {false} ∷ tail₁)

{-# TERMINATING #-}
listToType : List⁺ Expr -> Expr
listToType (head₁ ∷ []) = head₁
listToType (head₁ ∷ y ∷ tail) =
  functionApp head₁ (listToType (y ∷ tail)) {true}

emptyRange : Range
emptyRange = range 0 0

newHole : Expr
newHole = hole {""} {emptyRange} {[]} {[]}

sameId : Identifier -> Identifier -> Bool
sameId (identifier name isInRange scope declaration) (identifier name₁ isInRange₁ scope₁ declaration₁)
  with declaration Data.Nat.≟ declaration₁
sameId (identifier name isInRange scope declaration) (identifier name₁ isInRange₁ scope₁ declaration₁) | yes p = true
sameId (identifier name isInRange scope declaration) (identifier name₁ isInRange₁ scope₁ declaration₁) | no ¬p = false
