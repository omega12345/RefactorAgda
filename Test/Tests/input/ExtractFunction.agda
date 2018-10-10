{-# OPTIONS --allow-unsolved-metas #-}
module ExtractFunction where
open import Data.Nat

plus : ℕ -> ℕ -> ℕ
plus = {!   !}

function1 : (x : ℕ) -> (y : ℕ) -> ℕ
function1 x y = plus x y


pickTheFirst :  ℕ -> ℕ -> ℕ
pickTheFirst x y = x

function2 :  ℕ -> ℕ -> ℕ
function2 x y = pickTheFirst x y
