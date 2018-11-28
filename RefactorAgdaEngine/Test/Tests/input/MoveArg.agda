module moveArg where
open import Data.Nat
open import Data.Bool

stuff : ( number : ℕ) -> Bool -> Bool
stuff number bool = {!   !}

stuff2 : ℕ -> Bool -> Bool
stuff2 number bool = bool

nonsense : Bool
nonsense = stuff 0 true


dep : (A : Set) -> (B : A) -> Bool -> Bool
dep a b c = c
