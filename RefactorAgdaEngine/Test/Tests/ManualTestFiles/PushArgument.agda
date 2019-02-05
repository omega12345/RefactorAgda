module PushArgument where

open import Agda.Builtin.Nat

data List (A : Set) : Nat -> Set where
    nil : List A 0
    cons : {n : Nat} -> A -> List A n -> List A (suc n)

newFunc : {A : Set} -> {B : Set} -> (f : A -> B) -> (x : A) -> B

newFunc f x = f x

map : {n : Nat} -> {A : Set} -> {B : Set} -> (A -> B) -> List A n -> List B n

map f nil = nil

map f (cons x xs) = cons (newFunc f x) (map f xs)
