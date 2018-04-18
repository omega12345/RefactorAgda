{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Expression parser generator with bracket support.
module Brackets where

import Text.Megaparsec.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text

makeExprParserWithBrackets ::
  MonadParsec e s m => m b -> m c -> m d -> m a -> [[Operator m a]] -> m a
makeExprParserWithBrackets openingBracketParser closingBracketParser spaceConsumer termParser operatorTable =
  calculator
  where bracketParser = do
          openingBracketParser
          spaceConsumer
          x <- calculator
          spaceConsumer
          closingBracketParser
          return x
        calculator = makeExprParser (try (termParser) <|> bracketParser) operatorTable

makeExprParserWithParens ::
  MonadParsec e Text m =>  m d -> m a -> [[Operator m a]] -> m a
makeExprParserWithParens = makeExprParserWithBrackets
                           (string "(")
                           (string ")")
