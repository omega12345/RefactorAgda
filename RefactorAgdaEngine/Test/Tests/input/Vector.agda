open import Agda.Builtin.Nat


data Vec (A : Set) : Nat -> Set where
  [] : Vec A 0
  cons : {n : Nat} -> A -> Vec A n -> Vec A (suc n)

empty : Vec Nat 0
empty = []
