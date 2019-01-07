module Conversion where
open import Agda.Builtin.Nat

nonDependent : Nat -> Nat -> Nat
nonDependent a b = a

dependent : {A : Set} -> A -> A
dependent a = a

stuff : {A : Set} -> {B : Nat} -> Nat -> Nat
stuff zero = zero
stuff (suc c) = dependent c
