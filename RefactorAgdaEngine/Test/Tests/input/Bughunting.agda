module Bughunting where

data Nat : Set where zero : Nat
                     suc  : Nat -> Nat

plus  : Nat ->  Nat -> Nat

{-# BUILTIN NATURAL Nat #-}


plus  m n = {!   !}

data List2 (A : Set) : Nat -> Set where
      empty2 : List2 A 0
      cons2 : {n : Nat} -> A -> List2 A n -> List2 A (plus 1 n)
