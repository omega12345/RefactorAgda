module Typing where
import IO.Primitive as Prim
--open import Data.Unit
open import ParseTree
open import Data.Nat
open import Data.List
open import Data.String
open import Data.Unit
{-# FOREIGN GHC
  import InteractWithAgda
#-}

postulate getTypes : List ParseTree -> ℕ -> List Expr -> String -> Prim.IO (List Type)
{-# COMPILE GHC getTypes = getTypes #-}

postulate getEnvironment : List  ParseTree -> ℕ -> String -> Prim.IO (List TypeSignature)
{-# COMPILE GHC getEnvironment = getEnvironment #-}

postulate output : String -> Prim.IO ⊤
{-# COMPILE GHC output = output #-}
