module Refactoring where
open import ParseTree
open import Data.List
open import Data.String
open import Data.Bool
doNothing : List ParseTree -> List ParseTree
doNothing x = x

updateRange : Range -> Range
updateRange (range x x₁ x₂) = range x x₁ true

setNeedsUpdating : ParseTree -> ParseTree
setNeedsUpdating (signature x x₁) = signature x (updateRange x₁)
setNeedsUpdating (functionDefinition x x₁ x₂ x₃) = functionDefinition x x₁ x₂ (updateRange x₃)
setNeedsUpdating (dataStructure x x₁ x₂ x₃ x₄) = dataStructure x x₁ x₂ x₃ (updateRange x₄)
setNeedsUpdating (pragma x x₁) = pragma x (updateRange x₁)



reindentFunc : String -> ParseTree -> ParseTree
reindentFunc name a@(signature (typeSignature funcName _ comments) _) =
  if name == funcName then setNeedsUpdating a else a
reindentFunc name a@(functionDefinition definitionOf _ _ _) =
  if name == definitionOf then setNeedsUpdating a else a
reindentFunc _ x = x



-- reindents all constructs without making changes.
reindentFile : List ParseTree -> List ParseTree
reindentFile code = map setNeedsUpdating code

reindentFunction : String -> List ParseTree -> List ParseTree
reindentFunction text code = map (reindentFunc text) code

{-# COMPILE GHC doNothing as doNothing #-}
{-# COMPILE GHC reindentFile as reindentFile #-}
{-# COMPILE GHC reindentFunction as reindentFunction #-}
