module ExtractFunction where
open import Data.List
open import ParseTree
open import Data.Nat
open import Data.String hiding (_++_)
open import ScopeState using (ScopeState ; ScopeEnv ; replaceID ; liftIO ; sameId ; getUniqueIdentifier)
open import ScopeParseTree
open import AgdaHelperFunctions
open import Typing
open import Data.Bool
open import MatchUpNames
open import Category.Monad.State
open import Data.Product
open import Data.Maybe
open import Relation.Nullary
open import Data.Unit using (⊤ ; tt)
getIDForModule : List ParseTree -> ScopeState Identifier
getIDForModule [] = ScopeState.fail "This module seems to have no name"
getIDForModule (moduleName id range₁ ∷ p) = ScopeState.return id
getIDForModule (x ∷ xs) = getIDForModule xs

record ExtractionEnv : Set where
  constructor extEnv
  field
   stuffBeforeFunc : List ParseTree
   stuffAfterFunc : List ParseTree
   rightHandSideBuilder : Maybe (Expr -> Expr)
   extractedExpr : Maybe Expr
   holesPassed : ℕ
   functionRebuilder : Maybe (Expr -> ParseTree)

ExtractionState : Set -> Set
ExtractionState = StateT ExtractionEnv ScopeState

runExtractionState :
    {a : Set} -> ExtractionState a -> ExtractionEnv -> ScopeState a
runExtractionState eMonad e = do
      (result , newState) <- eMonad e
      return result
    where open RawMonadState (StateTMonadState ScopeEnv (SumMonadT IOMonad String))

liftScopeState : {a : Set} -> ScopeState a -> ExtractionState a
liftScopeState action ee = do
  a <- action
  return (a , ee)
  where open RawMonadState (StateTMonadState ScopeEnv (SumMonadT IOMonad String))

open RawMonadState (StateTMonadState ExtractionEnv (StateTMonad ScopeEnv (SumMonadT IOMonad String)))

fail : {a : Set} -> String -> ExtractionState a
fail s = liftScopeState $ ScopeState.fail s

--assuming that start < end
isInside : Range -> ℕ -> ℕ -> Bool
isInside (range lastUnaffected lastAffected) start end
  with end ≤? lastUnaffected | lastAffected ≤? suc start
... | no p | no q = true
... | p | q = false

findPartInExpr : Expr -> ℕ -> ℕ -> ExtractionState Bool
findPartInExpr (numLit {value}
    {r} {c} {c2}) start end
      with isInside r start end
... | true = do
      extEnv x y f expr h g  <- get
      put $ extEnv x y (just(λ a -> a)) (just $ numLit {value} {r} {c} {c2}) h g
      return true
... | false = return false
findPartInExpr (ident (identifier name isInRange scope declaration {c} {c2})) start end
  with isInRange start | isInRange end
... | after | _ = return  false
... | _ | before = return false
... | _ | _ = do
    extEnv x y f expr h g <- get
    put $ extEnv x y (just(λ a -> a)) (just $ ident $ identifier name isInRange scope declaration {c} {c2}) h g
    return true
findPartInExpr (hole {t} {p} {c} {c2}) start end
    with isInside p start end
... | true = do
  extEnv x y f expr h g  <- get
  put $ extEnv x y (just(λ a -> a)) (just $ hole {t} {p} {c} {c2}) h g
  return true
... | false = return false
findPartInExpr (functionApp e e₁) start end = do
    true <- findPartInExpr e₁ start end
      where false -> do
                  true <- findPartInExpr e start end
                    where false -> return false
                  extEnv x y (just f) expr h g <- get
                    where _ -> fail "Got no function despite extraction"
                  put $ extEnv x y (just(λ x -> functionApp (f x) e₁)) expr h g
                  return true
    true <- findPartInExpr e start end
      where false -> return true
    -- at this point we realize we want to extract the entire
    -- functionApp
    -- Therefore, we need to extract entire e and e₁ regardless of
    -- how far into it the selection extends.
    extEnv x y f expr h g <- get
    put $ extEnv x y (just (λ x -> x)) (just $ functionApp e e₁) h g
    return true

isIdInExpr : Identifier -> List Expr -> ExtractionState Bool
isIdInExpr i (ident identifier₁ ∷ e) = do
        if sameId i identifier₁
          then return true
          else isIdInExpr i e
isIdInExpr i (functionApp x x₁ ∷ e) = do
    a <- isIdInExpr i $ x ∷ []
    b <- isIdInExpr i $ x₁ ∷ []
    return $ a ∨ b
isIdInExpr _ _ = return false

-- returns a list of all arguments which must be passed to the new function.
identifiersUsed : List Expr -> Expr -> ExtractionState (List Expr)
identifiersUsed params numLit = return []
identifiersUsed params (ident identifier₁) = do
      true <- isIdInExpr identifier₁ params
        where false -> return []
      return $ ident identifier₁ ∷ []
identifiersUsed params hole = return []
identifiersUsed params (functionApp body body₁) = do
      a <- identifiersUsed params body
      b <- identifiersUsed params body₁
      return $ a Data.List.++ b

countHole : ExtractionState ⊤
countHole = do
  extEnv b a func exp h g <- get
  put $ extEnv b a func exp (suc h) g
  return tt

mapState : {A B : Set} -> (A -> ExtractionState B) -> List A -> ExtractionState (List B)
mapState f [] = return []
mapState f (x ∷ list) = do
  x1 <- f x
  xs <- mapState f list
  return (x1 ∷ xs)

countHolesInExpr : Expr -> ExtractionState ⊤
countHolesInExpr hole = countHole
countHolesInExpr (functionApp e e₁) = do
  countHolesInExpr e
  countHolesInExpr e₁
countHolesInExpr _ = return tt

countHolesInSignature : TypeSignature -> ExtractionState ⊤
countHolesInType : Type -> ExtractionState ⊤
countHolesInType (type expression) = countHolesInExpr expression
countHolesInType (namedArgument arg) = countHolesInSignature arg
countHolesInType (functionType t t₁) = do
  countHolesInType t
  countHolesInType t₁


countHolesInSignature (typeSignature funcName funcType) =
  countHolesInType funcType

countHolesInTree : ParseTree -> ExtractionState ⊤
countHolesInTree (signature signature₁ range₁) = countHolesInSignature signature₁
countHolesInTree (functionDefinition definitionOf params body range₁) = countHolesInExpr body
countHolesInTree (dataStructure dataName parameters indexInfo constructors range₁) = do
  mapState countHolesInSignature parameters
  mapState countHolesInSignature constructors
  countHolesInType indexInfo
countHolesInTree _ = return tt

findPartToExtract : List ParseTree -> ℕ -> ℕ -> ExtractionState (List Expr)
findPartToExtract [] start end = fail "Found nothing that can be extracted"
findPartToExtract (functionDefinition definitionOf params body (range a z) ∷ e)
      start end with (suc a) ≤? start | end ≤? z
... | yes p | yes q = do
      true <- findPartInExpr body start end
        where false -> fail "Command start and end must be in the function body"
      extEnv bef aft func exp h g <- get
      put $ extEnv bef e func exp h $ just (λ x -> functionDefinition definitionOf params x (range a z))
      just ex <- return exp
        where nothing -> fail "Didn't get an expression to extract"
      mapState countHolesInTree bef
      identifiersUsed params ex
findPartToExtract (p ∷ ps) start end | yes a | no b = do
    extEnv b a func exp h g  <- get
    put $ extEnv (b ∷ʳ p) a func exp h g
    findPartToExtract ps start end
... | p | q = fail "Command start and end not fully inside any function"
findPartToExtract (p ∷ ps) start end = do
    extEnv b a func exp h g <- get
    put $ extEnv (b ∷ʳ p) a func exp h g
    findPartToExtract ps start end

makeTypeFromTypes : List Type -> ExtractionState Type
makeTypeFromTypes [] = fail "Can't make a type from nothing!"
makeTypeFromTypes (x ∷ []) = return x
makeTypeFromTypes (x ∷ x2) = do
      t <- makeTypeFromTypes x2
      return $ functionType x t

makeExprFromExprs : List Expr -> ExtractionState Expr
makeExprFromExprs [] = fail "Can't make expr from nothing"
makeExprFromExprs (x ∷ []) = return x
makeExprFromExprs (x ∷ l) = do
  y <- makeExprFromExprs l
  return $ functionApp y x


getSignatureFor : Identifier -> List ParseTree -> ExtractionState ParseTree
getSignatureFor i [] = fail "Couldn't find type signature"
getSignatureFor i (signature (typeSignature funcName funcType) range₁ ∷ l)
  with sameId i funcName
... | true = return $ signature (typeSignature funcName funcType) range₁
... | false = getSignatureFor i l
getSignatureFor i (x ∷ xs) = getSignatureFor i xs

doExtraction : List ParseTree -> ℕ -> ℕ -> String -> ExtractionState (List ParseTree)
doExtraction program startPoint endPoint filename = do
  scoped <- liftScopeState $ scopeParseTreeList program
  -- TODO: Assuming that the module only has a simple name.
  -- Otherwise, need to calculate the right name.
  identifier name _ _ declaration <- liftScopeState $ getIDForModule scoped
  liftScopeState $ replaceID declaration "RefactorAgdaTemporaryFile"
  renamedInputProgram <- liftScopeState $ matchUpNames scoped
  expsUsed <- findPartToExtract renamedInputProgram startPoint endPoint
  extEnv b a (just expBuilder) (just extractedExp) h (just functionBuilder)  <- get
    where _ -> fail "An error in doExtraction"
  let programWithNewHole = b ++
        ((functionBuilder $ expBuilder $ hole {""} {range 0 0} {[]} {[]})∷ a)
  types <- liftScopeState $ liftIO $ getTypes programWithNewHole h
            (expsUsed ∷ʳ extractedExp) filename
  funcName <- liftScopeState $ getUniqueIdentifier
  let newFunction = functionDefinition funcName expsUsed extractedExp (range 0 0)
  newFunctionType <- makeTypeFromTypes types
  let newFunctionType = signature (typeSignature funcName newFunctionType) $ range 0 0
  replacingExpr <- makeExprFromExprs $ reverse $ ident funcName ∷ expsUsed
  let oldFunction = functionBuilder $ expBuilder $ replacingExpr
  let completeCode = b ++
        (newFunctionType ∷ newFunction ∷ oldFunction ∷ a)
  scopeCompleted <- liftScopeState $ scopeParseTreeList completeCode
  identifier _ _ _ d <- liftScopeState $ getIDForModule scopeCompleted
  liftScopeState $ replaceID d name
  liftScopeState $ matchUpNames scopeCompleted

extract : List ParseTree -> ℕ -> ℕ -> String -> ScopeState (List ParseTree)
extract p s e f = runExtractionState (doExtraction p s e f) $ extEnv [] [] nothing nothing 0 nothing
