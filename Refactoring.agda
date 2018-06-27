module Refactoring where
open import ParseTree
open import Data.List
open import Data.String hiding (_++_)
open import Data.Sum hiding (map)
open import Data.Product
open import ScopeParseTree
open import MatchUpNames
open import ScopeEnv
open import Data.Nat
open import InterpretCommand
open import Data.Maybe
open import Data.Vec using (toList  ; lookup ; Vec)
open import Data.Fin

doNothing : List ParseTree -> List ParseTree
doNothing x = x

{-# COMPILE GHC doNothing as doNothing #-}

scopeList : ScopeState (List ParseTree)
scopeList = do
  env {numVars} vars maxScopes scopes oldCurrent current <- get
  let decl = Data.Vec.map getDeclared scopes
  let scopeDec =  Data.Vec.map (λ x -> Data.List.map (λ y -> Data.Vec.lookup y vars) x) decl
  return (inj₁ (Data.String.concat (Data.List.map concatSpace (Data.Vec.toList scopeDec))))
  where concatSpace : List String -> String
        concatSpace l =  (Data.String.concat l) Data.String.++ "\n"

varList : ScopeState (List ParseTree)
varList = do
  env vars maxScopes scopes oldCurrent current <- get
  return (inj₁ (Data.String.concat (Data.Vec.toList vars)))


scopeOnly : List ParseTree -> String ⊎ List ParseTree
scopeOnly program =
  proj₁ (((matchUpNames after scopeParseTreeList) program ) newEnv)



{-# COMPILE GHC scopeOnly as scopeOnly #-}

checkCommand' : List ParseTree -> ℕ -> ScopeState String
checkCommand' list n = do
  let afterScoping = scopeOnly list
  (inj₂ newList) <- scopeParseTreeList list
    where (inj₁ x) -> return (inj₁ x)
  (just decl) <- return (getDeclarationIDForPoint newList n)
    where (nothing) -> return (inj₁ "Cursor is not in renameable instance")
  getNameForId decl

checkCommand : List ParseTree -> ℕ -> String ⊎ String
checkCommand list n = proj₁ ((checkCommand' list n ) newEnv)

{-# COMPILE GHC checkCommand as checkCommand #-}

rename' : List ParseTree -> ℕ -> String -> ScopeState (List ParseTree)
rename' program point newName = do
    (inj₂ scoped) <- scopeParseTreeList program
      where (inj₁ x) -> return (inj₁ x)
    (just decl) <- return (getDeclarationIDForPoint scoped point)
      where nothing -> return (inj₁ "Cursor is not in renameable instance")
    (inj₂ noerror) <- replaceID decl newName
      where (inj₁ x) -> return (inj₁ x)
    matchUpNames scoped


rename : List ParseTree -> ℕ -> String -> String ⊎ List ParseTree
rename program point newName = proj₁ ((rename' program point newName) newEnv)

{-# COMPILE GHC rename as rename #-}
