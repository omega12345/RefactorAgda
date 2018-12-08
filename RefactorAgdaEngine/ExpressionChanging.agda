module ExpressionChanging where
open import ParseTree
open import Data.Maybe hiding (map)
open import Data.List.NonEmpty
open import ParseTreeOperations
open import AgdaHelperFunctions
open import Data.List

{-# TERMINATING #-}
applyToExpressionsAndSubExpressions : (List⁺ Expr -> List⁺ Expr) -> Expr -> Expr
applyToExpressionsAndSubExpressions i e = listToExpression $ head newTop ∷ newRest
    where newTop = i  $ expressionToList e
          newRest = Data.List.map (applyToExpressionsAndSubExpressions i) $ tail newTop

applyToAllInSignature : (List⁺ Expr -> List⁺ Expr) -> TypeSignature -> TypeSignature
applyToAllInSignature i (typeSignature funcName funcType) = typeSignature funcName $ applyToExpressionsAndSubExpressions i funcType

applyToAllExpressions : (List⁺ Expr -> List⁺ Expr) -> ParseTree -> ParseTree
applyToAllExpressions instructions (signature signature₁ range₁) =
  signature (applyToAllInSignature instructions signature₁) range₁
applyToAllExpressions instructions (functionDefinition definitionOf params body range₁) =
  functionDefinition definitionOf (tail newParams)
  (applyToExpressionsAndSubExpressions instructions body) range₁
    where newParams = instructions (ident definitionOf ∷ Data.List.map (applyToExpressionsAndSubExpressions instructions) params)


applyToAllExpressions instructions (dataStructure dataName parameters indexInfo constructors range₁ {c} ) =
  dataStructure
  dataName
  (Data.List.map (applyToAllInSignature instructions) parameters)
  (applyToExpressionsAndSubExpressions instructions indexInfo)
  (Data.List.map (applyToAllInSignature instructions) constructors)
  range₁ {c}
applyToAllExpressions instructions x = x


applyToProgram : (List⁺ Expr -> List⁺ Expr) -> Data.List.List ParseTree -> Data.List.List ParseTree
applyToProgram i c = Data.List.map (applyToAllExpressions i) c
