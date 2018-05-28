-- a few random examples
module Test where
import Data.Bool

data Nat : Set where zero : Nat -- Comment which gets eaten
                     suc  : Nat -> Nat --Comment which is preserved
plus {- preserved comment  {- which may be nested -} -} :
{- comment after Colon, also preserved-}
{-comments are essentially whitespace, even through they get parsed-} Nat -> {-comment, not preserved-} Nat -> Nat
{-# BUILTIN NATURAL
Nat #-}

plus  zero   m = m
plus (suc n) m = suc (plus n m)

{-# BUILTIN NATPLUS plus #-}


--excess brackets are not preserved
minus : ((Nat)) -> Nat
-- We can nest holes correctly
minus a = {! ? {! Hole inside hole !}  !}

-- Seriously long and complicated function type
func : {A : Set} ->
 {B : A -> Set} ->
 {C : (x : A) -> B x -> Set}  ->
 (f : {x : A} -> (y : B x) -> C x y) ->
 (g : (x : A) -> B x) ->
 (x : A) ->
 C x (g x)
func f g x = f (g x)

-- Holes can go here as well
id : {a : Set} -> a -> {!   !}
id x = x

-- and the code from the planning report, rewritten in Baby-Agda

data List (A : Set) : Set where
      empty : List A
      cons : A -> List A -> List A

append : {A : Set} -> List A -> List A -> List A
append empty ys = ys
append (cons x xs) ys = cons x (append xs ys)

-- proposed code after refactoring

data List2 (A : Set) : Nat -> Set where
      empty2 : List2 A 0
      cons2 : {n : Nat} -> A -> List2 A n -> List2 A (plus 1 n)

append2 : {n : Nat} -> {m : Nat} -> {A : Set} ->
            List2 A m -> List2 A n -> List2 A (plus m n)
append2 empty2 ys = ys
append2 (cons2 x xs) ys = cons2 x (append2 xs ys)
