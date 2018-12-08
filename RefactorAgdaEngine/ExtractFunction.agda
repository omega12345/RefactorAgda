module ExtractFunction where
open import Data.List
open import ParseTree
open import Data.Nat
open import Data.String hiding (_++_)
open import ScopeState using (ScopeState ; ScopeEnv ; replaceID ; liftIO ; getUniqueIdentifier)
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
open import Data.Nat.Show
open import ParseTreeOperations

--TODO: actually, it is okay if the module has no name - in this case we do not need to rename it.
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
   oldFunctionName : Maybe Identifier

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

isInIdent : Identifier -> ℕ -> ℕ -> Bool
isInIdent (identifier name isInRange scope declaration {b}{c} {c2}) start end
  with isInRange start | isInRange end
... | after | _ = false
... | _ | before = false
... | _ | _ = true

findPartInExpr : Expr -> ℕ -> ℕ -> ExtractionState Bool
findPartInExpr (numLit {value}
    {r} {c} {c2}) start end
      with isInside r start end
... | true = do
      extEnv x y f expr h g n  <- get
      put $ extEnv x y (just(λ a -> a)) (just $ numLit {value} {r} {c} {c2}) h g n
      return true
... | false = return false
findPartInExpr (ident id) start end
  with isInIdent id start end
... | false = return false
... | true = do
    extEnv x y f expr h g n <- get
    put $ extEnv x y (just(λ a -> a)) (just $ ident id) h g n
    return true
findPartInExpr (hole {t} {p} {c} {c2}) start end
    with isInside p start end
... | true = do
  extEnv x y f expr h g n  <- get
  put $ extEnv x y (just(λ a -> a)) (just $ hole {t} {p} {c} {c2}) h g n
  return true
... | false = return false
findPartInExpr (functionApp e e₁ {false}) start end = do
    true <- findPartInExpr e₁ start end
      where false -> do
                  true <- findPartInExpr e start end
                    where false -> return false
                  -- here we want to extract some subexpression of e
                  -- so we rebuild it with rebuilder f and put it in the functionApp
                  extEnv x y (just f) expr h g n <- get
                    where _ -> fail "Got no function despite extraction"
                  put $ extEnv x y (just(λ x -> functionApp (f x) e₁ {false})) expr h g n
                  return true
    extEnv x y (just f) expr h g n <- get
      where _ -> fail "Got no function despite extraction"
    true <- findPartInExpr e start end
      where false -> do -- extract only (part of) e₁
                put $ extEnv x y (just (λ a -> functionApp e (f a) {false})) expr h g n
                return true
    -- at this point we realize we want to extract the entire
    -- functionApp
    -- Therefore, we need to extract entire e and e₁ regardless of
    -- how far into it the selection extends.
    --extEnv x y f expr h g n <- get
    put $ extEnv x y (just (λ x -> x)) (just $ functionApp e e₁ {false}) h g n
    return true
findPartInExpr (functionApp e e₁ {true}) start end = do
  true <- findPartInExpr e start end
    where false -> do
           true <- findPartInExpr e₁ start end
            where false -> return false
           extEnv x y (just f) expr h g n <- get
            where _ -> fail "Got no function despite extraction"
           put $ extEnv x y (just(λ x -> functionApp e (f x) {true})) expr h g n
           return true
  extEnv x y (just f) expr h g n <- get
   where _ -> fail "Got no function despite extraction"
  namedArgument sign <- return e
   where _ -> do
          -- e is not a named argument so can be extracted by itself
          true <- findPartInExpr e₁ start end
           where false -> do
                  put $ extEnv x y (just (λ a -> functionApp (f e) e₁ {true})) expr h g n
                  return true
          put $ extEnv x y (just (λ x -> x)) (just $ functionApp e e₁ {true}) h g n
          return true
  put $ extEnv x y (just $ λ x -> x) (just $ functionApp e e₁ {true}) h g n
  return true
findPartInExpr (implicit x) start end = do
  true <- findPartInExpr x start end
    where false -> return false
  extEnv x y (just f) expr h g n <- get
    where _ -> fail "Extraction failed"
  put $ extEnv x y (just(λ a -> implicit $ f a)) expr h g n
  return true
findPartInExpr (underscore {p} {c1} {c2}) start end with isInside p start end
... | true = do
  extEnv x y f expr h g n  <- get
  put $ extEnv x y (just(λ a -> a)) (just $ underscore {p} {c1} {c2}) h g n
  return true
... | false = return false
findPartInExpr (namedArgument (typeSignature funcName funcType) {b}) start end
  with isInIdent funcName start end
-- can't extract just the name of a variable
... | true =  do
            extEnv x y f expr h g n <- get
            put $ extEnv x y (just $ λ x -> x)
              (just $ namedArgument (typeSignature funcName funcType)  {b}) h g n
            return true
... | false = do
    true <- findPartInExpr funcType start end
      where false -> return false
    extEnv x y (just f) expr h g n  <- get
      where _ -> fail "No function in state"
    put $ extEnv x y (just $ λ x -> namedArgument (typeSignature funcName $ f x) {b}) expr h g n
    return true

sameNameAndStatus : Identifier -> Identifier -> Bool
sameNameAndStatus (identifier name isInRange scope declaration {inScope1}) (identifier name₁ isInRange₁ scope₁ declaration₁ {inScope2}) = (name == name₁) ∧ (not $ inScope1 xor inScope2)

isIdInExpr : (whatToSearchFor : Identifier) -> (whereToSearch : Expr) -> Bool
isIdInExpr i (ident identifier₁) = sameNameAndStatus i identifier₁
isIdInExpr i (functionApp x x₁) = isIdInExpr i x ∨ isIdInExpr i x₁
isIdInExpr _ _ = false

countHole : ExtractionState ⊤
countHole = do
  extEnv b a func exp h g n <- get
  put $ extEnv b a func exp (suc h) g n
  return tt

mapState : {A B : Set} -> (A -> ExtractionState B) -> List A -> ExtractionState (List B)
mapState f [] = return []
mapState f (x ∷ list) = do
  x1 <- f x
  xs <- mapState f list
  return (x1 ∷ xs)

countHolesInSignature : TypeSignature -> ExtractionState ⊤

countHolesInExpr : Expr -> ExtractionState ⊤
countHolesInExpr hole = countHole
countHolesInExpr (functionApp e e₁) = do
  countHolesInExpr e
  countHolesInExpr e₁
countHolesInExpr (namedArgument arg) = countHolesInSignature arg
countHolesInExpr _ = return tt





countHolesInSignature (typeSignature funcName funcType) =
  countHolesInExpr funcType

countHolesInTree : ParseTree -> ExtractionState ⊤
countHolesInTree (signature signature₁ range₁) = countHolesInSignature signature₁
countHolesInTree (functionDefinition definitionOf params body range₁) = countHolesInExpr body
countHolesInTree (dataStructure dataName parameters indexInfo constructors range₁) = do
  mapState countHolesInSignature parameters
  mapState countHolesInSignature constructors
  countHolesInExpr indexInfo
countHolesInTree _ = return tt

findPartToExtract : List ParseTree -> ℕ -> ℕ -> ExtractionState ⊤
findPartToExtract [] start end = fail "Found nothing that can be extracted"
findPartToExtract (functionDefinition definitionOf params body (range a z) ∷ e)
      start end with (suc a) ≤? start | end ≤? z
... | yes p | yes q = do
      true <- findPartInExpr body start end
        where false -> fail "Command start and end must be in the function body"
      extEnv bef aft func exp h g n <- get
      put $ extEnv bef e func exp h (just (λ x -> functionDefinition definitionOf params x (range a z))) $ just definitionOf
      just ex <- return exp
        where nothing -> fail "Didn't get an expression to extract"
      mapState countHolesInTree bef
      return tt
findPartToExtract (p ∷ ps) start end | yes a | no b = do
    extEnv b a func exp h g n  <- get
    put $ extEnv (b ∷ʳ p) a func exp h g n
    findPartToExtract ps start end
... | p | q = fail "Command start and end not fully inside any function"
findPartToExtract (p ∷ ps) start end = do
    extEnv b a func exp h g n <- get
    put $ extEnv (b ∷ʳ p) a func exp h g n
    findPartToExtract ps start end

makeTypeFromTypes : List Expr -> ExtractionState Expr
makeTypeFromTypes [] = fail "Can't make type from nothing"
makeTypeFromTypes (x ∷ []) = return x
makeTypeFromTypes (x ∷ xs) = do
    y <- makeTypeFromTypes xs
    return $ functionApp x y {true}

makeExprFromExprs : List Expr -> ExtractionState Expr
makeExprFromExprs [] = fail "Can't make expr from nothing"
makeExprFromExprs (x ∷ []) = return x
makeExprFromExprs (x ∷ l) = do
  y <- makeExprFromExprs l
  return $ functionApp y x {false}

-- List of type signatures is environment. Contains all variables,
-- including not-in-scope. Return list is the new function's environment.
filterEnvironment : Expr -> List TypeSignature -> (extractedExp : Expr) -> ExtractionState (List TypeSignature)
filterEnvironment resultType [] varsUsed = return []
filterEnvironment resultType (typeSignature funcName funcType ∷ environment) extractedExpr = do
  filteredEnv <- filterEnvironment resultType environment extractedExpr
  let isInExtractedExpr = isIdInExpr funcName extractedExpr
  let typesToCheck = Data.List.map (λ { (typeSignature n t) -> t}) filteredEnv
  isInTypes <- mapState (findInType funcName) typesToCheck
  let isIn = or isInTypes
  if isIn ∨ isInExtractedExpr
   then return $ (typeSignature funcName funcType) ∷ filteredEnv
   else return filteredEnv
 where
  findInType : Identifier -> Expr -> ExtractionState Bool

  findInType id (namedArgument (typeSignature funcName funcType)) = findInType id funcType
  findInType id (functionApp t t₁ {true}) = do
    one <- findInType id t
    two <- findInType id t₁
    return $ one ∨ two
  findInType id expression = return $ isIdInExpr id expression

bringIdInScope : Identifier -> Identifier
bringIdInScope (identifier name isInRange scope declaration {inScope} {a} {b}) = identifier name isInRange scope declaration {true} {a}{b}

containsIdInSign : TypeSignature -> Identifier -> Bool

containsInType : Expr -> Identifier -> Bool

containsInType (namedArgument arg) i = containsIdInSign arg i
containsInType (functionApp t t₁ {true}) i = containsInType t i ∨ containsInType t₁ i
containsInType expression i = isIdInExpr i expression

containsIdInSign (typeSignature name funcType) id = sameNameAndStatus name id ∨ containsInType funcType id

isInScope : Identifier -> Bool
isInScope (identifier _ _ _ _ {isInScope}) = isInScope

bringExprInScope : Expr -> Expr
bringExprInScope (ident id) = ident $ bringIdInScope id
bringExprInScope (functionApp e e₁ {b}) = functionApp (bringExprInScope e) (bringExprInScope e₁) {b}
bringExprInScope (implicit e) = implicit $ bringExprInScope e
bringExprInScope x = x

bringInScope : Expr -> Expr
bringSignatureInScope : TypeSignature -> TypeSignature
bringSignatureInScope (typeSignature funcName funcType) =
  typeSignature (bringIdInScope funcName) $ bringInScope funcType



bringInScope (namedArgument arg {b}) = namedArgument (bringSignatureInScope arg) {b}
bringInScope (functionApp t t₁ {true}) = functionApp (bringInScope t) (bringInScope t₁) {true}
bringInScope expression = bringExprInScope expression
renameNotInScopeExpr : Identifier -> Identifier -> Expr -> Expr
renameNotInScopeExpr (identifier name₁ isInRange₁ scope₁ declaration₁) to (ident (identifier name isInRange scope declaration {false} {b} {a})) with name == name₁
renameNotInScopeExpr (identifier name₁ isInRange₁ scope₁ declaration₁) (identifier name₂ isInRange₂ scope₂ declaration₂) (ident (identifier name isInRange scope declaration {false} {b} {a})) | true = ident $ identifier name₂ isInRange₂ scope₂ declaration₂ {false}{b} {a}
... | false = ident $ identifier name isInRange scope declaration {false} {b} {a}
renameNotInScopeExpr from to (functionApp e e₁ {b}) =
  functionApp (renameNotInScopeExpr from to e) (renameNotInScopeExpr from to e₁) {b}
renameNotInScopeExpr from to (implicit e) = implicit $ renameNotInScopeExpr from to e
renameNotInScopeExpr _ _ x = x

renameNotInScopeOfName : Identifier -> Identifier -> Expr -> Expr

renameNotInScopeSign : Identifier -> Identifier -> TypeSignature -> TypeSignature
renameNotInScopeSign from to (typeSignature funcName funcType) =
  typeSignature funcName $ renameNotInScopeOfName from to funcType



renameNotInScopeOfName from to (namedArgument arg {b}) = namedArgument  (renameNotInScopeSign from to arg) {b}
renameNotInScopeOfName from to (functionApp signs signs₁ {true}) =
  functionApp (renameNotInScopeOfName from to signs) (renameNotInScopeOfName from to signs₁) {true}
renameNotInScopeOfName from to expression = renameNotInScopeExpr from to expression

-- returns list of types, which are explicit or implicit named arguments
-- and a list of expressions which is the left-hand side of the new function, i.e. idents.
signatureToType : Expr -> List TypeSignature -> ExtractionState (List Expr × List Expr)
signatureToType result [] = return $ (result ∷ []) , []
signatureToType result (typeSignature funcName funcType ∷ laterSigns) = do
  let inScopeType = bringInScope funcType
  (restOfSigns , exprs) <- signatureToType result laterSigns
  false <- return $ isInScope funcName
    where true -> do -- in this case, we plainly don't want to rename anything
                    let newSign = namedArgument (typeSignature funcName funcType) {true}
                    return $ newSign ∷  restOfSigns , ident funcName ∷ exprs
  -- Do we need to rename the current variable?
  true <- return $ or $ Data.List.map (λ x -> containsIdInSign x $ bringIdInScope funcName) laterSigns
    where false -> do
              let newSign = namedArgument (typeSignature funcName funcType) {false}
              return $ newSign ∷  restOfSigns , exprs
  liftScopeState $ liftIO $ output "Need to rename a variable"
  newName <- liftScopeState getUniqueIdentifier
  let renamedSigns = Data.List.map (renameNotInScopeOfName funcName newName) restOfSigns
  return $ namedArgument (typeSignature newName inScopeType) {false} ∷ renamedSigns , exprs

placeInRightPlace : Identifier -> (placeOverTypeSignature : Bool) -> ParseTree -> ParseTree -> List ParseTree -> List ParseTree
placeInRightPlace oldFunctionName _ newsign newdef [] = newsign ∷ newdef ∷ []
placeInRightPlace oldFunctionName false newsign newdef (functionDefinition definitionOf params body range₁ ∷ program) with sameId oldFunctionName definitionOf
...| false = functionDefinition definitionOf params body range₁ ∷ placeInRightPlace oldFunctionName false newsign newdef program
... | true = newsign ∷ newdef ∷ functionDefinition definitionOf params body range₁ ∷ program
placeInRightPlace oldFunctionName true newsign newdef (signature (typeSignature funcName funcType) range₁ ∷ program) with sameId oldFunctionName funcName
... | false = signature (typeSignature funcName funcType) range₁ ∷ placeInRightPlace oldFunctionName true newsign newdef program
... | true = newsign ∷ newdef ∷  signature (typeSignature funcName funcType) range₁ ∷ program
placeInRightPlace x y z a (p ∷ program) = p ∷ placeInRightPlace x y z a program

doExtraction : List ParseTree -> ℕ -> ℕ -> String -> ExtractionState (List ParseTree)
doExtraction program startPoint endPoint filename = do
  scoped <- liftScopeState $ scopeParseTreeList program
  -- TODO: Assuming that the module only has a simple name.
  -- Otherwise, need to calculate the right name.
  identifier name _ _ declaration <- liftScopeState $ getIDForModule scoped
  liftScopeState $ replaceID declaration "RefactorAgdaTemporaryFile"
  renamedInputProgram <- liftScopeState $ matchUpNames scoped
  findPartToExtract renamedInputProgram startPoint endPoint
  extEnv b a (just expBuilder) (just extractedExp) h (just functionBuilder) (just oldFunctionName)  <- get
    where _ -> fail "An error in doExtraction"
  let programWithNewHole = b ++
        ((functionBuilder $ expBuilder $ hole {""} {range 0 0} {[]} {[]})∷ a)
  (resultTypeNewFunc ∷ []) <- liftScopeState $ liftIO $ getTypes
                              programWithNewHole h
                                (extractedExp ∷ []) filename
            where _ -> fail "Something happened in InteractWithAgda"
  environment <- liftScopeState $ liftIO $ getEnvironment programWithNewHole h filename
  newFuncName <- liftScopeState $ getUniqueIdentifier
  newFuncEnv <- filterEnvironment resultTypeNewFunc environment extractedExp
  (newFuncSignParts , newFuncArgNames) <- signatureToType resultTypeNewFunc newFuncEnv
  newFuncType <- makeTypeFromTypes $ newFuncSignParts
  let newFuncSignature = signature (typeSignature newFuncName newFuncType) (range 0 0)
  let newFuncDefinition = functionDefinition newFuncName newFuncArgNames extractedExp (range 0 0)
  replacingExpr <- makeExprFromExprs $ reverse $ ident newFuncName ∷ newFuncArgNames
  let oldFunction = functionBuilder $ expBuilder $ replacingExpr
  let placeBelowTypeSignature = isIdInExpr oldFunctionName extractedExp
  let completeCode = placeInRightPlace oldFunctionName (not placeBelowTypeSignature) newFuncSignature newFuncDefinition b ++
        (oldFunction ∷ a)
  scopeCompleted <- liftScopeState $ scopeParseTreeList completeCode
  identifier _ _ _ d <- liftScopeState $ getIDForModule scopeCompleted
  liftScopeState $ replaceID d name
  liftScopeState $ matchUpNames scopeCompleted

extract : List ParseTree -> ℕ -> ℕ -> String -> ScopeState (List ParseTree)
extract p s e f = runExtractionState (doExtraction p s e f) $ extEnv [] [] nothing nothing 0 nothing nothing
