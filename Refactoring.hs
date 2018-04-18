{-# LANGUAGE NamedFieldPuns #-}
module Refactoring where
import Data.Text (Text)
import ParseTree

-- reindents all constructs without making changes.
reindentFile :: [ParseTree] -> [ParseTree]
reindentFile code = map setNeedsUpdating code

reindentFunction :: Text -> [ParseTree] -> [ParseTree]
reindentFunction text code = map (reindentFunc text) code

reindentFunc :: Text -> ParseTree -> ParseTree
reindentFunc name a@(Signature {signature = TypeSignature {funcName}})
  | name == funcName = setNeedsUpdating a
reindentFunc name a@(FunctionDefinition{definitionOf})
  | name == definitionOf = setNeedsUpdating a
reindentFunc _ x = x

setNeedsUpdating :: ParseTree -> ParseTree
setNeedsUpdating tree = tree {range = (range tree) {needsUpdating = True}}
