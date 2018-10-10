module Typing where
import IO.Primitive as Prim
--open import Data.Unit
open import ParseTree
open import Data.Nat
open import Data.List
open import Data.String
{-# FOREIGN GHC
  import InteractWithAgda
#-}

postulate getTypes : List ParseTree -> ℕ -> List Expr -> String -> Prim.IO (List Type)
{-# COMPILE GHC getTypes = getTypes #-}
