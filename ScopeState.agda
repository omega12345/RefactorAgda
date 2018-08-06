-- Home to all functions creating, modifying or removing ScopeEnv or ScopeState
{-# OPTIONS --allow-unsolved-metas #-}
module ScopeState where
open import Data.List using (List) renaming ([] to emptyList ; map to listMap ; _∷_ to _cons_)
open import Data.String renaming (_++_ to _then_)
open import Data.Bool
open import Data.Nat
open import Data.Fin renaming (zero to fzero ; suc to fsuc) hiding (_+_)
open import Relation.Binary.PropositionalEquality
open import Data.Maybe renaming (map to maybemap) hiding (monad)
open import ParseTree
open import Data.List.Properties
open import Data.Vec hiding (_>>=_) renaming (lookup to veclookup)
open import Category.Monad.State
open import Data.Product hiding (map)
open import Data.Sum hiding (map)
open import Data.Empty
open import Relation.Nullary
open import Category.Monad
open import Relation.Binary.Core
open import Relation.Binary.PropositionalEquality.TrustMe
open import Data.Unit

data ScopeType : Set where
  funcDef : ScopeType
  moduleDef : (name : Identifier) -> ScopeType
  addFuncToModule : ScopeType
  addVariableToType : ScopeType
  topLevel : ScopeType

data Scope (maxVars : ℕ) (maxScopes : ℕ): Set where
 mkScope : (scopeType : ScopeType) ->
           (enclosing : Maybe (Fin maxScopes)) -> -- Nothing if this is the top scope
           (declaredVars : List (Fin maxVars)) -> -- the list of variables declared in this scope
           Scope maxVars maxScopes

getType : {m n : ℕ} -> Scope m n -> ScopeType
getType (mkScope scopeType enclosing declaredVars) = scopeType

--appendToDeclared : {m n : ℕ} -> Fin m -> Scope m n -> Scope m n
--appendToDeclared what (mkScope scopeType enclosing declaredVars) =
--  mkScope scopeType enclosing (what cons declaredVars)

getDeclared : {m n : ℕ} -> Scope m n -> List (Fin m)
getDeclared (mkScope scopeType enclosing declaredVars) = declaredVars

data ScopeEnv : Set where
  env : {numVars : ℕ} ->
        (vars : Vec String numVars) ->
        (maxScopes : ℕ) ->
        (scopes : Vec (Scope numVars maxScopes) maxScopes ) ->
        (current : Fin maxScopes) ->
        ScopeEnv

ScopeState : Set -> Set
ScopeState = StateT  (ScopeEnv) (String ⊎_)

bind : {x y : Set} -> String ⊎ x -> (x -> String ⊎ y) -> String ⊎ y
bind (inj₁ x) f = inj₁ x
bind (inj₂ y) f = f y

SumMonad : RawMonad (String ⊎_)
SumMonad  = record
  { return  = inj₂
  ; _>>=_ = bind
  }

open RawMonadState (StateTMonadState ScopeEnv {M = String ⊎_} SumMonad) public

run : {a : Set} ->  ScopeState a -> ScopeEnv -> (String ⊎ a)
run s e = convert (s e)
  where convert : {a : Set} -> String ⊎ (Σ a (λ x -> ScopeEnv)) -> String ⊎ a
        convert (inj₁ x) = inj₁ x
        convert (inj₂ (fst , snd)) = inj₂ fst

fail : {a : Set} -> String -> ScopeState a
fail message = λ _ → inj₁ message

try : {a : Set} -> ScopeState a -> ScopeState (String ⊎ a)
try st = do
  environment <- get
  inj₂ (x , newEnv) <- return (st environment)
    where inj₁ error -> return (inj₁ error)
  put newEnv
  return (inj₂ x)

newEnv : ScopeEnv
newEnv = env [] 1 ((mkScope topLevel nothing emptyList) ∷ [])   fzero

-- Variable adding

newRaise : {n : ℕ} -> Fin n -> Fin (1 + n)
newRaise fzero = fzero
newRaise (fsuc f) = fsuc (newRaise f)

raiseScopeType : {n m : ℕ} -> Scope n m -> Scope (1 + n) m
raiseScopeType (mkScope t s declared) = mkScope t s (listMap newRaise declared)

raiseType : {n m : ℕ} -> (x : Scope n m) -> getType x ≡ getType (raiseScopeType x)
raiseType (mkScope scopeType enclosing declaredVars) = refl

mapRaiseType : {n m o : ℕ} -> (x : Vec (Scope n m) o) -> (curr : Fin o)
  -> getType (veclookup curr x) ≡ getType (veclookup curr (map raiseScopeType x))
mapRaiseType (x ∷ xs) fzero with veclookup fzero (x ∷ xs)
... | c = raiseType c
mapRaiseType (x ∷ xs) (fsuc curr) = mapRaiseType xs curr

addVarToScope : {n m : ℕ} -> (Scope n m) -> (what : Fin (1 + n))
          -> (Scope (1 + n) m)
addVarToScope (mkScope scopeType enclosing declaredVars) what =
    mkScope scopeType enclosing (what cons (listMap newRaise declaredVars))

addVar : {n m numScopes : ℕ}(xs : Vec (Scope n m) numScopes) ->
  (atPos : Fin numScopes) -> (what : Fin (1 + n)) ->
    Vec (Scope (1 + n) m) numScopes
addVar (x ∷ xs) fzero what = addVarToScope x what ∷ map raiseScopeType xs
addVar (x ∷ xs) (fsuc atPos) what = raiseScopeType x ∷ addVar xs atPos what

addExistingVarToScope : {n m : ℕ} -> (Scope n m) -> (what : Fin n)
          -> (Scope n m)
addExistingVarToScope (mkScope scopeType enclosing declaredVars) what =
    mkScope scopeType enclosing (what cons declaredVars)

addExistingVar : {n m numScopes : ℕ}(xs : Vec (Scope n m) numScopes) ->
  (atPos : Fin numScopes) -> (what : Fin n) ->
    Vec (Scope n m) numScopes
addExistingVar [] () what
addExistingVar (x ∷ xs) fzero what = (addExistingVarToScope x what) ∷ xs
addExistingVar (x ∷ xs) (fsuc atPos) what = x ∷ addExistingVar xs atPos what

-- returns input identifier with id and scope id filled in
addIdentifier :  Identifier -> ScopeState Identifier
addIdentifier (identifier name isInRange scope declaration) = do
  env {numVars} vars maxScopes scopes current <- get
  let newScopes = (addVar scopes current (fromℕ numVars))
  put (env (vars ∷ʳ name) maxScopes
     newScopes
       current)
  return (identifier name isInRange (toℕ current) numVars)

raiseEnclosingScope : {maxVars maxScopes : ℕ} -> Scope maxVars maxScopes
    -> Scope maxVars (1 + maxScopes)
raiseEnclosingScope (mkScope scopeType enclosing declaredVars) =
    mkScope scopeType (maybemap newRaise enclosing) declaredVars

addScope : (b : ScopeType) -> (s : ScopeEnv ) → String ⊎ Σ ℕ (λ x → ScopeEnv)
addScope scopeType = do
  env vars maxScopes scopes current <- get
  let newScopes = map raiseEnclosingScope scopes ∷ʳ
                   mkScope scopeType
                    (just (newRaise current)) emptyList
  put (env vars (1 + maxScopes)
                      newScopes
                      (fromℕ maxScopes)
                       )
  return maxScopes

castCurrent : {n m : ℕ} -> {A : Set} -> Vec A n -> Fin m -> ScopeState (Fin n)
castCurrent [] x = fail "Something went wrong with scope saving"
castCurrent (x ∷ vec) fzero = return fzero
castCurrent (x ∷ vec) (fsuc f) = do
  r <- castCurrent vec f
  return (fsuc r)

saveAndReturnToScope : {A : Set} -> ScopeState A -> ScopeState A
saveAndReturnToScope ss = do
  env vars maxScopes scopes current <- get
  c <- ss
  env vars₁ maxScopes₁ scopes₁ current₁ <- get
  newCurr <- castCurrent scopes₁ current
  let newEnv = env vars₁ maxScopes₁ scopes₁ newCurr
  put newEnv
  return c

setScope : (n : ℕ) -> (s : ScopeEnv ) → String ⊎ Σ ℕ (λ x → ScopeEnv)
setScope n (env vars maxScopes scopes current) with suc n Data.Nat.≤? maxScopes
setScope n (env vars maxScopes scopes current) | yes p = inj₂ (n , (env vars maxScopes scopes (fromℕ≤ p)))
setScope n e | no ¬p = inj₁ "Tried to set scope to invalid value"

filter : {A : Set} -> (A -> Bool) -> List A -> List A
filter f emptyList = emptyList
filter f (x cons l) = if (f x) then (x cons (filter f l)) else filter f l

-- this is the lookup function from Schäfer's algorithm
-- TODO: Simple dummy function, does not sufficiently reflect real
-- scoping rules
-- TODO: Agda does not understand that this terminates because
-- the fact that the enclosing scope must be farther to the front of
-- the scopes list is not encoded in the type yet
-- returns the declaration id the input string
{-# TERMINATING #-}
lookup : String -> ScopeState ℕ
lookup s = saveAndReturnToScope (do
  env vars maxScopes scopes current <- get
  mkScope scopeType enclosing decl <- return (veclookup current scopes)
  emptyList <- return ( filter (λ x -> veclookup x vars == s) decl)
    where x cons xs -> return (toℕ x)
  just n <- return enclosing
    where _ -> fail "There is no enclosing scope"
  setScope (toℕ n)
  lookup s)

-- fill in the declaration and scope information in an identifier
-- given that the name has already been declared

fillInIdentifier : Identifier -> ScopeState Identifier
fillInIdentifier (identifier name isInRange scope declaration) = do
  x <- saveAndReturnToScope (lookup name)
  suc n <- return x
    where zero -> fail ("Identifier not found in file: " then name)
  env vars maxScopes scopes current <- get
  return (identifier name isInRange (toℕ current) (suc n))

-- at the place where something is declared, you can only use simple names.
-- TODO: Actually, this should be adequately bounded by the current scope. But how to implement?

access : Identifier -> ScopeState Identifier
access (identifier name isInRange scope declaration) = do
  env {numVars} vars maxScopes scopes current <- get
  yes p <- return (suc declaration Data.Nat.≤? numVars)
    where _ -> fail "Parse tree scoping has produced a nonsense declaration"
  let newName = veclookup (fromℕ≤ p) vars
  anything <- setScope scope
  identifier n r s d <- fillInIdentifier (identifier newName isInRange scope declaration)
  yes x <- return (d Data.Nat.≟ declaration)
    where no y -> fail "Could not perform name change because this would change the meaning of the code"
  return (identifier n r s d)

mapState : {A B : Set} -> (A -> ScopeState B) -> List A -> ScopeState (List B)
mapState f emptyList = return emptyList
mapState f (x cons list) = do
  x1 <- f x
  xs <- mapState f list
  return (x1 cons xs)

addContentReferenceToModuleTop : TypeSignature -> ScopeState ℕ
addContentReferenceToModuleTop (typeSignature (identifier name isInRange scope 0) funcType comments) = fail "Trying to add signature which has not been scoped"
addContentReferenceToModuleTop (typeSignature (identifier name isInRange scope declaration) funcType comments) = do
  env {numVars } vars maxScopes scopes current <- get
  yes p <- return (suc declaration Data.Nat.≤? numVars)
    where _ -> fail "Found messed-up scoping"
  let newScopes = addExistingVar scopes current (fromℕ≤ p)
  return declaration

_after_ : {A B C : Set} -> (B -> ScopeState C) -> (A -> ScopeState B) -> A -> ScopeState C
_after_ f g x = do
  prelim <- g x
  f prelim

getNameForId : ℕ -> ScopeState String
getNameForId n = do
  env {numVars } vars maxScopes scopes current <- get
  yes p <- return (suc n Data.Nat.≤? numVars)
    where _ -> fail "Can't get name for this invalid identifier"
  let r = veclookup (fromℕ≤ p) vars
  return r

replaceName : {A : Set} -> {n : ℕ} -> Fin n -> A -> Vec A n -> Vec A n
replaceName fzero a (x ∷ list) = a ∷ list
replaceName (fsuc n) a (x ∷ list) = x ∷ replaceName n a list

replaceID : ℕ -> String -> ScopeState String
replaceID which newName = do
  env {numVars} vars maxScopes scopes current <- get
  yes p <- return (suc which Data.Nat.≤? numVars)
    where _ -> fail "Can't rename this invalid identifier"
  let newList = replaceName (fromℕ≤ p) newName vars
  put (env {numVars } newList maxScopes scopes current)
  return newName

currentScopeTypeIsFuncDef : ScopeState Bool
currentScopeTypeIsFuncDef = do
  env {numVars } vars maxScopes scopes current <- get
  mkScope funcDef enclosing declaredVars <- return (veclookup current scopes)
    where _ -> fail "This is not the scope type you expected"
  return true
-- 291, 12070
