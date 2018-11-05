{-# OPTIONS --allow-unsolved-metas #-}
module ExtractDependent where
open import Data.Nat
open import Data.Bool
open import Data.String

apply : (A : Set) -> (B : A -> Set) ->
            ((x : A) -> B x) -> (a : A) -> B a
apply A B f a = f a


applySameName : (A : Set) -> (A : Set) -> (B : A -> Set) ->
             (h : Set) -> (h : (x : A) -> B x) -> (a : A) -> B a
applySameName C A B g f a = f a

-- TODO : Try same test with {A} {B} once the parser can handle that.
applyImp : {A : Set} -> {B : A -> Set} ->
            ((x : A) -> B x) -> (y : A) -> B y
applyImp f a = f a

applyImpSameName : {A : Set} -> {A : Set} -> {B : A -> Set} ->
             (h : Set) -> (h : (x : A) -> B x) -> (a : A) -> B a
applyImpSameName A B h = B h
