module BabyAgda where
data Nat : Set where
    zero : Nat
    suc : Nat -> Nat
    stuff : Nat

nonDependent : Nat -> Nat -> Nat
nonDependent a b = a

dependent : {A : Set} -> A -> A
dependent a = a

asdf : {A : Set} -> {B : Nat} -> {renameMe0 : Nat} -> Nat
asdf {_} {_} {zero} = zero
asdf {_} {_} {suc c} = dependent c
