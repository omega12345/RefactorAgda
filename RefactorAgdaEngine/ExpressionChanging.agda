module ExpressionChanging where
open import ParseTree
open import Data.Maybe hiding (map)
open import Data.List.NonEmpty
open import ParseTreeOperations
open import AgdaHelperFunctions
open import Data.List
open import Data.Nat
open import Data.Vec using (Vec)
open import Data.Fin
open import ScopeState
import Data.Nat.Properties
open import Relation.Nullary
import Data.String
open import Data.Nat.Show
open import Data.Bool

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

makeFin : {n : ℕ} -> Vec Expr (n) -> ℕ -> ScopeState (Fin n)
makeFin {n} v number with number Data.Nat.Properties.<? n
makeFin {n} v number | yes p = return $ fromℕ≤ p
makeFin {n} v number | no ¬p = fail $ "Got a non-existing argument: " Data.String.++ Data.Nat.Show.show n Data.String.++ Data.Nat.Show.show (Data.List.length $ Data.Vec.toList v)

makeFin2 : {n : ℕ} -> Vec Expr (suc n) -> ℕ -> ScopeState (Fin n)
makeFin2 {n} v number with number Data.Nat.Properties.<? n
makeFin2 {n} v number | yes p = return $ fromℕ≤ p
makeFin2 {n} v number | no ¬p = fail $ "Got a non-existing argument: " Data.String.++ Data.Nat.Show.show n Data.String.++ Data.Nat.Show.show (Data.List.length $ Data.Vec.toList v)


--TODO: try to give this a better type
updateSignature :  ℕ -> List⁺ Expr ->
  (alterSignature : {n : ℕ} -> Vec Expr (suc n)  -> Fin n -> ScopeState (Vec Expr (suc n))) ->
  ScopeState (List⁺ Expr)
updateSignature argNo type f
  with toVec type
... | c = do
-- In a signature, we always have at least one item which is not an argument: the result type
  newFin <- makeFin2 c argNo
  newVec <- f c newFin
  return  $ Data.List.NonEmpty.fromVec newVec

doForArgument : List ParseTree -> ℕ -> ℕ ->
  (alterSignature : {n : ℕ} -> Vec Expr (suc n)  -> Fin n -> ScopeState (Vec Expr (suc n))) ->
  (alterExpression : TypeSignature -> ℕ -> ScopeState (List⁺ Expr -> List⁺ Expr)) ->
  ScopeState (List ParseTree)
doForArgument [] funcID whichArgument f1 f2 = fail "funcId not found in doForArgument"
doForArgument (signature (typeSignature (identifier name isInRange scope declaration) funcType) range₁ ∷ code) funcID whichArgument f1 f2 with Data.Nat.compare declaration funcID
doForArgument (signature (typeSignature (identifier name isInRange scope declaration {e}{b} {a}) funcType) range₁ ∷ code) .declaration whichArgument f1 f2 | equal .declaration = do
    newType <- updateSignature whichArgument (typeToList funcType) f1
    instruction <- f2 ((typeSignature (identifier name isInRange scope declaration {e}{b} {a})) funcType) whichArgument
    return $ applyToProgram instruction $
      signature (typeSignature
                  (identifier name isInRange scope declaration {e}{b} {a})
                  (listToType newType)) range₁ ∷ code
doForArgument (signature (typeSignature (identifier name isInRange scope declaration {e}{b} {a}) funcType) range₁ ∷ code) funcID whichArgument f1 f2 | _ = do
  newxs <- doForArgument code funcID whichArgument f1 f2
  return $ (signature (typeSignature (identifier name isInRange scope declaration {e}{b} {a}) funcType) range₁) ∷ newxs
doForArgument (x ∷ xs) funcID whichArgument f1 f2 = do
  newxs <- doForArgument xs funcID whichArgument f1 f2
  return $ x ∷ newxs

makeInstruction : TypeSignature -> ℕ -> (makeIns : {n : ℕ} -> Vec Expr n -> Fin n -> (List Expr -> List Expr)) -> ScopeState (List⁺ Expr -> List⁺ Expr)
makeInstruction (typeSignature funcName funcType) argNo f
  with toVec (typeToList funcType)
... | c = do
    newFin <- makeFin c argNo
    return $ function $ f c newFin
    where function : (List Expr -> List Expr) -> (List⁺ Expr -> List⁺ Expr)
          function f (ident identifier₁ ∷ tail₁)
              with sameId identifier₁ funcName
          function f (ident identifier₁ ∷ tail₁) | false =
            (ident identifier₁ ∷ tail₁)
          function f (ident identifier₁ ∷ tail₁) | true =
              ident identifier₁ ∷ f tail₁
          function f list = list
