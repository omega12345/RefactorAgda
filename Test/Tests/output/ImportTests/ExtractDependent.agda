module ImportTests.ExtractDependent where
open import Data.Bool
open import Data.Nat
open import ExtractDependent



f : ℕ -> Bool
f 0 = false
f _ = true

testApply : Bool
testApply = apply _ _ f 5

testApplyImp : Bool
testApplyImp = applyImp f 6

testApplyImpSameName : Bool
testApplyImpSameName = applyImpSameName {ℕ} Bool f 3
