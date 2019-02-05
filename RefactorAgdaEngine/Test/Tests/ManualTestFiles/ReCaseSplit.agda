module ReCaseSplit where

data Bool : Set where
  true : Bool
  false : Bool
  maybe : Bool

_func : Bool -> Bool
maybe func = ?
false func  = false
(true) func  = true


func2 : Bool -> Bool
func2 = λ{false → false ; (true) → true}

open import IO.Primitive

func3 : IO Bool
func3 = return false

func4 : IO Bool
func4 = do
  false <- func3
    where true -> return true
  return false

module SubModule where

 func5 : Bool -> Bool
 func5 maybe = ?
 func5 false = false
 func5 true =  func6 true
    where func6 : Bool -> Bool
          func6 maybe = ?
          func6 false = false
          func6 true = true
