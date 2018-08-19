module Refactoring where
open import ParseTree
open import Data.List
open import Data.String hiding (_++_)
open import Data.Sum hiding (map)
open import Data.Product
open import ScopeParseTree
open import MatchUpNames
open import ScopeState
open import Data.Nat
open import InterpretCommand
open import Data.Maybe
open import Data.Vec using (toList  ; lookup ; Vec)
open import Data.Fin
open import AgdaHelperFunctions
open import PushArgument renaming (pushArgument to push)


doNothing : List ParseTree -> List ParseTree
doNothing x = x

{-# COMPILE GHC doNothing as doNothing #-}

scopeList : ScopeState (List ParseTree)
scopeList = do
  env {numVars} vars maxScopes scopes current <- get
  let decl = Data.Vec.map getDeclared scopes
  let scopeDec =  Data.Vec.map ( λ x -> Data.List.map (λ y -> Data.Vec.lookup y vars) x) decl
  fail $ Data.String.concat $ Data.List.map concatSpace $ Data.Vec.toList scopeDec
  where concatSpace : List String -> String
        concatSpace l =  (Data.String.concat l) Data.String.++ "\n"

varList : ScopeState (List ParseTree)
varList = do
  env vars maxScopes scopes current <- get
  fail $ Data.String.concat $ Data.Vec.toList vars


scopeOnly : List ParseTree -> String ⊎ List ParseTree
scopeOnly program = run (matchUpNames after scopeParseTreeList $ program) newEnv

{-# COMPILE GHC scopeOnly as scopeOnly #-}

checkCommand' : List ParseTree -> ℕ -> ScopeState String
checkCommand' list n = do
  let afterScoping = scopeOnly list
  newList <- scopeParseTreeList list
  just decl <- return $ getDeclarationIDForPoint newList n
    where nothing -> fail "Cursor is not in renameable instance"
  getNameForId decl

checkCommand : List ParseTree -> ℕ -> String ⊎ String
checkCommand list n = run (checkCommand' list n ) newEnv

{-# COMPILE GHC checkCommand as checkCommand #-}

rename' : List ParseTree -> ℕ -> String -> ScopeState (List ParseTree)
rename' program point newName = do
    scoped <- scopeParseTreeList program
    just decl <- return $ getDeclarationIDForPoint scoped point
      where nothing -> fail "Cursor is not in renameable instance"
    noerror <- replaceID decl newName
    matchUpNames scoped


rename : List ParseTree -> ℕ -> String -> String ⊎ List ParseTree
rename program point newName = run (rename' program point newName) newEnv

{-# COMPILE GHC rename as rename #-}

pushArgument' : List ParseTree -> ℕ -> ScopeState (List ParseTree)
pushArgument' program point = do
  scoped <- scopeParseTreeList program
  funcID , argNumber <- getFuncIdAndArgNumber scoped point
  push scoped funcID argNumber

pushArgument : List ParseTree -> ℕ -> String ⊎ List ParseTree
pushArgument program point = run (pushArgument' program point) newEnv

{-# COMPILE GHC pushArgument as pushArgument #-}
