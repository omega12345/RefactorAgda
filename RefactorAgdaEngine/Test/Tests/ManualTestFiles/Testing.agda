module Testing where

data Nat : Set where
    zero : Nat
    suc : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}

plus : Nat -> Nat -> Nat
plus zero n = n
plus (suc m) n = suc (plus m n)

times : Nat -> Nat -> Nat
times zero n = zero
times (suc m) n = plus n (times m n)

data List (A : Set) : Nat -> Set where
    nil : List A 0
    cons : {n : Nat} -> A -> List A n -> List A (suc n)

f : Nat -> (b : Nat) -> (a : Nat) -> Nat
f x = plus

-- Push argument doesn't work with implicit
-- Toggle implicit generates code that the tool can't handle
-- also, want to include {something = x} in language

map : {n : Nat} -> {A : Set} -> {B : Set} -> {renameMe0 : A -> B} -> List A n ->
    List B n
map {renameMe0 = f} nil = nil
map {renameMe0 = f} (cons x xs) = cons (f x) (map {_} {_} {_} {f} xs)

map2 : {n : Nat} -> {A : Set} -> {B : Set} -> (A -> B) -> List A n ->
    List B n
map2 f nil = nil
map2 f (cons x xs) = cons (f x) (map2 f xs)
{-
data Eq {A : Set} (x : A) : A -> Set where
  refl : Eq x x

subst : {A : Set} -> (P : A -> Set) ->
        {x : A} -> {y : A} -> Eq x y -> P x -> P y
subst P refl px = px

data Foo : Set where
  foo : Foo
  bar : Foo

-- Refactoring to introduce these!
{-# FOREIGN GHC data Foo = Foo | Bar #-}
{-# COMPILE GHC Foo = data Foo (Foo | Bar) #-}
-}
