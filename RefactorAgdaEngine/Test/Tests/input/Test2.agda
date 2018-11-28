data Nat : Set where
  zero : Nat -- Comment which gets eaten
  suc  : Nat -> Nat

{-# BUILTIN NATURAL
Nat #-}


f : Nat -> Nat
f a = 3

g : Nat -> Nat
g a = 4

--TODO found first bug

function : Nat -> (Nat -> Nat) -> Nat
function a f = f a

apply : Nat
apply = function 2 g
