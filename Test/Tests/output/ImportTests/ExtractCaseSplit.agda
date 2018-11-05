module ImportTests.ExtractCaseSplit where
open import  ExtractCaseSplit
open import Data.Maybe
open import Data.Bool

test : Bool
test = func (just false)
