module ImportTests.ExtractFunction where

open import ExtractFunction
open import Data.Nat
open import Data.Bool
checkFunction1 : ℕ
checkFunction1 = function1 2 3


checkFunction2 : ℕ
checkFunction2 = function2 4 true
