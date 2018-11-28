{-# OPTIONS --allow-unsolved-metas #-}
module ExtractFunction where
open import Data.Nat
open import Data.Bool

plus : ℕ -> ℕ -> ℕ
plus = {!   !}

function1 : (x : ℕ) -> (y : ℕ) -> ℕ
function1 x y = plus x y


pickTheFirst :  ℕ -> Bool -> ℕ
pickTheFirst x y = x

function2 :  ℕ -> Bool -> ℕ
function2 x y = pickTheFirst x y
