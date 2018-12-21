module ExtractCaseSplit where

open import Data.Maybe
open import Agda.Builtin.Bool

not : Bool -> Bool
not true = false
not false = true

func : Maybe Bool -> Bool

func nothing = false
func (just x) = not x


open import Data.List

func2 : List Bool -> Bool
func2 [] = true
func2 (x ∷ x₁) = not (func2 x₁)
