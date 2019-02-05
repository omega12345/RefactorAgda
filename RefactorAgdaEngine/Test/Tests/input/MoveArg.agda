module moveArg where
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool

stuff : ( number : Nat) -> Bool -> Bool
stuff number bool = {!   !}

stuff2 : Nat -> Bool -> Bool
stuff2 number bool = bool

nonsense : Bool
nonsense = stuff 0 true


dep : (A : Set) -> (B : A) -> Bool -> Bool
dep a b c = c


unnamedDep : (A : Set) -> A -> Bool -> Bool
unnamedDep a b c = c

sameName : (A : Set) -> {A : Set} -> A -> A
sameName set a = a


nonDep : (A : Set) -> (B : Set) -> B -> B
nonDep set1 set2 b = b

data List (A : Set) : Nat -> Set where
    nil : List A 0
    cons : {n : Nat} -> A -> List A n -> List A (suc n)

map : {n : Nat} -> {A : Set} -> {B : Set} -> (A -> B) -> List A n -> List B n
map f nil = nil
map f (cons x xs) = cons (f x) (map f xs)

map2 : {A : Set} -> {n : Nat} -> {B : Set} -> (A -> B) -> List A n -> List B n
map2 f nil = nil
map2 f (cons x xs) = cons (f x) (map2 f xs)
