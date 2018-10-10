{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Expression parser generator with bracket support.
module Brackets where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Data.Text
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer

makeExprParserWithBrackets ::
  MonadParsec e s m => m b -> m c -> m d -> m a -> [[Operator m a]] -> m a
makeExprParserWithBrackets openingBracketParser closingBracketParser spaceConsumer termParser operatorTable = calculator
  where bracketParser = do
          openingBracketParser
          spaceConsumer
          x <- calculator
          spaceConsumer
          closingBracketParser
          return x
        calculator = makeExprParser (try termParser <|> bracketParser) operatorTable

makeExprParserWithParens ::
  MonadParsec e Text m =>  m d -> m a -> [[Operator m a]] -> m a
makeExprParserWithParens = makeExprParserWithBrackets
                           (string "(")
                           (string ")")

doBlockLikeStructure :: MonadParsec e s m => m a -> m b -> Bool -> m () -> m (a, [b])
doBlockLikeStructure doPart item atLeastOneItem space = do
  beforeDo <- indentLevel
  doResult <- doPart
  space --move until the start of whatever is in the do block
  indent <- indentLevel
  if indent > beforeDo
  then do foundItems <- itemsAtLevel (unPos indent) atLeastOneItem item space
          return (doResult, foundItems)
  else return (doResult, [])

itemsAtLevel :: MonadParsec e s m => Int -> Bool -> m a -> m () -> m [a]
--switch between many and some here to control whether items are needed
itemsAtLevel level atLeastOneItem item space =
  let line = try $ itemAtLevel level item space
  in if atLeastOneItem
      then some line
      else many line

itemAtLevel :: MonadParsec e s m => Int -> m a -> m () -> m a
itemAtLevel level item space = do
                       space --eat any whitespace
                       currentIndent <- indentLevel
                       if unPos currentIndent == level
                       then item
                       else incorrectIndent EQ (mkPos level) currentIndent
