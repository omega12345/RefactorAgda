module RenameParameter where
open import Data.String
data Bool : Set where
  true : Bool
  false : Bool


func : (b : Bool) -> {Bool : Bool -> Set} -> Bool b
func = {!   !}
