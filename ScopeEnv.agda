-- Home to all functions creating, modifying or removing ScopeEnv

module ScopeEnv where
open import Data.List using (List) renaming ([] to emptyList ; map to listMap ; _∷_ to _cons_)
open import Data.String renaming (_++_ to _then_)
open import Data.Bool
open import Data.Nat
open import Data.Fin renaming (zero to fzero ; suc to fsuc) hiding (_+_)
open import Relation.Binary.PropositionalEquality
open import Data.Maybe renaming (map to maybemap) hiding (monad)
open import ParseTree
open import Data.List.Properties
open import Data.Vec hiding (_>>=_)
open import Category.Monad.State
open import Data.Product hiding (map)
open import Data.Sum hiding (map)
open import Data.Empty
open import Relation.Nullary

data ScopeType : Set where
  funcDef : ScopeType
  moduleDef : (name : Identifier) -> ScopeType
  addFuncToModule : ScopeType
  addVariableToType : ScopeType
  topLevel : ScopeType

-- TODO: There are plenty of restrictions on which scopes may enclose which others, but how to encode?

data Scope (maxVars : ℕ) (maxScopes : ℕ): Set where
 mkScope : (scopeType : ScopeType) ->
           (enclosing : Maybe (Fin maxScopes)) -> -- Nothing if this is the top scope
           (declaredVars : List (Fin maxVars)) -> -- the list of variables declared in this scope
           Scope maxVars maxScopes

getType : {m n : ℕ} -> Scope m n -> ScopeType
getType (mkScope scopeType enclosing declaredVars) = scopeType

getEnclosing : {m n : ℕ} -> Scope m n -> Maybe (Fin n)
getEnclosing (mkScope scopeType enclosing declaredVars) = enclosing

getDeclared : {m n : ℕ} -> Scope m n -> List (Fin m)
getDeclared (mkScope scopeType enclosing declaredVars) = declaredVars

appendToDeclared : {m n : ℕ} -> Fin m -> Scope m n -> Scope m n
appendToDeclared what (mkScope scopeType enclosing declaredVars) =
  mkScope scopeType enclosing (what cons declaredVars)

data ScopeEnv : Set where
  env : {numVars : ℕ} ->
        (vars : Vec String numVars) ->
        (maxScopes : ℕ) ->
        (scopes : Vec (Scope numVars maxScopes) maxScopes ) ->
        (rememberedScopes : List (Fin maxScopes)) ->
        (current : Fin maxScopes) ->
        ScopeEnv

ScopeState : Set -> Set
ScopeState x = State ScopeEnv (String ⊎ x)

open RawMonadState (StateMonadState ScopeEnv) public


newEnv : ScopeEnv
newEnv = env [] 1 ((mkScope topLevel nothing emptyList) ∷ [])  emptyList fzero

-- Variable adding

newRaise : {n : ℕ} -> Fin n -> Fin (1 + n)
newRaise fzero = fzero
newRaise (fsuc f) = fsuc (newRaise f)

raiseScopeType : {n m : ℕ} -> Scope n m -> Scope (1 + n) m
raiseScopeType (mkScope t s declared) = mkScope t s (listMap newRaise declared)

addVarToScope : {n m : ℕ} -> (Scope n m) -> (what : Fin (1 + n))
          -> (Scope (1 + n) m)
addVarToScope (mkScope scopeType enclosing declaredVars) what =
    mkScope scopeType enclosing (what cons (listMap newRaise declaredVars))

addVar : {n m numScopes : ℕ}(xs : Vec (Scope n m) numScopes) ->
  (atPos : Fin numScopes) -> (what : Fin (1 + n)) ->
    Vec (Scope (1 + n) m) numScopes
addVar [] () what
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
addIdentifier : Identifier -> ScopeState Identifier
addIdentifier (identifier name isInRange scope declaration) = do
  env {numVars} vars maxScopes scopes oldCurrent current <- get
  put (env (vars ∷ʳ name) maxScopes
    (addVar scopes current (fromℕ numVars)) oldCurrent
       current)
  return (inj₂ (identifier name isInRange (toℕ current) numVars))

raiseEnclosingScope : {maxVars maxScopes : ℕ} -> Scope maxVars maxScopes
    -> Scope maxVars (1 + maxScopes)
raiseEnclosingScope (mkScope scopeType enclosing declaredVars) =
    mkScope scopeType (maybemap newRaise enclosing) declaredVars

-- returns the unique identifying number assigned to this scope
-- and sets the current scope to the new scope
-- means the same thing:
-- addScope : ScopeType -> ScopeEnv -> Σ ℕ (λ _ → ScopeEnv)
addScope : ScopeType -> ScopeState ℕ
addScope scopeType (env vars maxScopes scopes oldCurrent current)
  = inj₂ maxScopes , env vars (1 + maxScopes)
    (map raiseEnclosingScope scopes ∷ʳ mkScope scopeType (just (newRaise current)) emptyList) (listMap newRaise oldCurrent)
  (fromℕ maxScopes)

rememberScope : ScopeState ℕ
rememberScope (env vars maxScopes scopes oldCurrent current) =
  inj₂ (toℕ current) , env vars maxScopes scopes (current cons oldCurrent) current

returnToRememberedScope : ScopeState ℕ
returnToRememberedScope = do
  env vars maxScopes scopes (x cons oldCurrent) current <- get
   where _ -> return (inj₁ "Tried to return to a non-remembered scope")
  put (env vars maxScopes scopes oldCurrent x)
  return (inj₂ (toℕ x))

setScope : ℕ -> ScopeState ℕ
setScope n = do
  env vars maxScopes scopes rememberedScopes current <- get
  yes p <- return ((suc n) Data.Nat.≤? maxScopes)
    where _ -> return (inj₁ "Tried to set current scope to invalid scope")
  put (env vars maxScopes scopes rememberedScopes (fromℕ≤ p))
  return (inj₂ n)

reassemble : {A B : Set} -> ( A -> B ) -> String ⊎ A ->  ScopeState B
reassemble f (inj₁ x) = return (inj₁ x)
reassemble f (inj₂ y) = return (inj₂ (f y))

reassemble2 : {A B C : Set} -> (A -> B -> C) -> String ⊎ A -> String ⊎ B -> ScopeState C
reassemble2 f (inj₁ x) _ = return (inj₁ x)
reassemble2 f _ (inj₁ x) = return (inj₁ x)
reassemble2 f (inj₂ y) (inj₂ y₁) = return (inj₂(f y y₁))

filter : {A : Set} -> (A -> Bool) -> List A -> List A
filter f emptyList = emptyList
filter f (x cons l) = if (f x) then (x cons (filter f l)) else filter f l

-- moves up one step
moveToEnclosingScope : ScopeState ℕ
moveToEnclosingScope = do
  env vars maxScopes scopes oldCurrent current <- get
  just x <- return (getEnclosing (lookup current scopes))
    where _ -> return (inj₁ "There is no enclosing scope")
  yes p <- return (x Data.Fin.<? current)
    where _ -> return (inj₁ "There's a bug in the scoping algorithm: enclosing scope is not smaller than current scope")
  put (env vars maxScopes scopes oldCurrent x)
  return (inj₂ (toℕ x))

-- this is the lookup function from Schäfer's algorithm
-- TODO: Simple dummy function, does not sufficiently reflect real
-- scoping rules
-- TODO: Agda does not understand that this terminates because
-- the fact that the enclosing scope must be farther to the front of
-- the scopes list is not encoded in the type yet
-- returns the declaration id the input string
{-# TERMINATING #-}
findDeclaration : String -> ScopeState ℕ
findDeclaration s = do
  env vars maxScopes scopes oldCurrent current <- get
  let curr = lookup current scopes
  let decl = getDeclared curr
  emptyList <- return ( filter (λ x -> lookup x vars == s) decl)
    where x cons xs -> return (inj₂ (toℕ x))
  inj₂ x <- moveToEnclosingScope
    where inj₁ y -> return (inj₁ y)
  findDeclaration s

-- fill in the declaration and scope information in an identifier
-- given that the name has already been declared

fillInIdentifier : Identifier -> ScopeState Identifier
fillInIdentifier (identifier name isInRange scope declaration) = do
  rememberScope
  x <- findDeclaration name
  returnToRememberedScope
  inj₂ (suc n) <- return x
    where inj₁ b -> return (inj₁ b)
          inj₂ zero -> return (inj₁ ("Identifier not found in file: " then name) )
  env vars maxScopes scopes oldCurrent current <- get
  return (inj₂ (identifier name isInRange (toℕ current) (suc n)))

-- at the place where something is declared, you can only use simple names.

renameDeclaringIdentifier : Identifier -> ScopeState Identifier
renameDeclaringIdentifier (identifier name isInRange scope declaration) = do
  env {numVars} vars maxScopes scopes oldCurrent current <- get
  yes p <- return (suc declaration Data.Nat.≤? numVars)
    where _ -> return (inj₁ "Parse tree scoping has produced a nonsense declaration")
  let newName = lookup (fromℕ≤ p) vars
  inj₂ anything <- setScope scope
    where inj₁ x -> return (inj₁ x)
  inj₂ (identifier n r s d) <- fillInIdentifier (identifier newName isInRange scope declaration)
    where inj₁ x -> return (inj₁ x)
  yes x <- return (d Data.Nat.≟ declaration)
    where no y -> return (inj₁ "Could not perform name change because this would change the meaning of the code" )
  return (inj₂ (identifier n r s d))

-- go from decl id and scope to a suitable name
-- TODO: Pretty much dummy function, which only picks the current simple name.
-- We'll have to assume that scope and declaration are valid, because telling
-- Agda that would be a job!
-- TODO: This needs to be changed once renameIdentifier starts returning more sophisticated results
renameIdentifier : Identifier -> ScopeState Identifier
renameIdentifier = renameDeclaringIdentifier

mapState : {A B : Set} -> (A -> ScopeState B) -> List A -> ScopeState (List B)
mapState f emptyList = return (inj₂ emptyList)
mapState f (x cons list) = do
  inj₂ x1 <- f x
    where inj₁ z -> return (inj₁ z)
  inj₂ xs <- mapState f list
    where inj₁ z -> return (inj₁ z)
  return (inj₂(x1 cons xs))

getIdentifierName : Identifier -> String
getIdentifierName (identifier name isInRange scope declaration) = name

getIdentifierDeclaration : Identifier -> ℕ
getIdentifierDeclaration (identifier name isInRange scope declaration) = declaration

addContentReferenceToModuleTop : TypeSignature -> ScopeState ℕ
addContentReferenceToModuleTop (typeSignature (identifier name isInRange scope 0) funcType comments) = return (inj₁ "Trying to add signature which has not been scoped")
addContentReferenceToModuleTop (typeSignature (identifier name isInRange scope declaration) funcType comments) = do
  env {numVars } vars maxScopes scopes oldCurrent current <- get
  yes p <- return (suc declaration Data.Nat.≤? numVars)
    where _ -> return (inj₁ "Found messed-up scoping")
  let newScopes = addExistingVar scopes current (fromℕ≤ p)
  return (inj₂ declaration)

_after_ : {A B C : Set} -> (B -> ScopeState C) -> (A -> ScopeState B) -> A -> ScopeState C
_after_ f g x = do
  inj₂ prelim <- g x
    where inj₁ string -> return (inj₁ string)
  f prelim

getNameForId : ℕ -> ScopeState String
getNameForId n = do
  env {numVars } vars maxScopes scopes oldCurrent current <- get
  yes p <- return (suc n Data.Nat.≤? numVars)
    where _ -> return (inj₁ "Can't get name for this invalid identifier")
  let r = lookup (fromℕ≤ p) vars
  return (inj₂ r)

replaceName : {A : Set} -> {n : ℕ} -> Fin n -> A -> Vec A n -> Vec A n
replaceName fzero a (x ∷ list) = a ∷ list
replaceName (fsuc n) a (x ∷ list) = x ∷ replaceName n a list

replaceID : ℕ -> String -> ScopeState String
replaceID which newName = do
  env {numVars} vars maxScopes scopes oldCurrent current <- get
  yes p <- return (suc which Data.Nat.≤? numVars)
    where _ -> return (inj₁ "Can't rename this invalid identifier")
  let newList = replaceName (fromℕ≤ p) newName vars
  put (env {numVars } newList maxScopes scopes oldCurrent current)
  return (inj₂ newName)

currentScopeTypeIsFuncDef : ScopeState Bool
currentScopeTypeIsFuncDef = do
  env {numVars } vars maxScopes scopes oldCurrent current <- get
  mkScope funcDef enclosing declaredVars <- return (lookup current scopes)
    where _ -> return (inj₁ "This is not the scope type you expected")
  return (inj₂ true)
