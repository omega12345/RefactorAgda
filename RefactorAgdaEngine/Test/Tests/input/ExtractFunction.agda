{-# OPTIONS --allow-unsolved-metas #-}
module ExtractFunction where
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool

plus : Nat -> Nat -> Nat
plus = {!   !}

function1 : (x : Nat) -> (y : Nat) -> Nat
function1 x y = plus x y


pickTheFirst :  Nat -> Bool -> Nat
pickTheFirst x y = x

function2 :  Nat -> Bool -> Nat
function2 x y = pickTheFirst x y
