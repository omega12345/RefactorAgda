open import Data.Nat


data Vec (A : Set) : ℕ -> Set where
  [] : Vec A 0
  cons : {n : ℕ} -> A -> Vec A n -> Vec A (n + 1)

empty : Vec ℕ 0
empty = []
