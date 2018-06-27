module Bughunting2 where

data Nat : Set where zero : Nat -- Comment which gets eaten
                     suc  : Nat -> Nat --Comment which is preserved

plus  :
 Nat ->  Nat -> Nat

plus2 : Nat -> Nat -> Nat
plus2 = plus

plus zero  m = m
plus m n = {!   !}
