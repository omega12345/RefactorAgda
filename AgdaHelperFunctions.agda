-- stuff I did not find in the standard library, but which
-- I might just not have noticed.

module AgdaHelperFunctions where

open import Data.String
open import Data.Sum
open import Category.Monad

infixr 0 _$_
_$_ : {A B : Set} -> (A -> B) -> A -> B
x $ y = x y

bind : {error x y : Set} -> error ⊎ x -> (x -> error ⊎ y) -> error ⊎ y
bind (inj₁ x) f = inj₁ x
bind (inj₂ y) f = f y

SumMonad : {error : Set} -> RawMonad (error ⊎_)
SumMonad  = record
  { return  = inj₂
  ; _>>=_ = bind
  }

bindT : {monadType : Set -> Set}(underlyingMonad : RawMonad monadType){error x y : Set} -> (monadType (error ⊎ x)) -> (x -> monadType (error ⊎ y)) -> monadType (error ⊎ y)
bindT underlyingMonad x f = do
    inj₂ content <- x
      where inj₁ e -> return $ inj₁ e
    f content
  where open RawMonad underlyingMonad

SumMonadT : {monadType : Set -> Set}(underlyingMonad : RawMonad monadType)(error : Set) -> RawMonad (λ x -> monadType (error ⊎ x))
SumMonadT underlyingMonad error = record
  { return = λ x -> r $ inj₂ x
  ; _>>=_ = bindT underlyingMonad
  }
  where open RawMonad underlyingMonad renaming (return to r)

import IO.Primitive as Prim

IOMonad : RawMonad (λ (x : Set) -> Prim.IO x)
IOMonad = record
  { return = Prim.return
  ; _>>=_ = Prim._>>=_
  }
