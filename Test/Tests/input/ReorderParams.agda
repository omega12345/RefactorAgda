module ReorderParams where
open import Agda.Builtin.Nat



id : (A : Set) -> (a : A) -> A
id A a = a

id2 : (A : Set) -> (a : A) -> A
id2 = id
