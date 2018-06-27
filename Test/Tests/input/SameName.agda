module SameName where

data Stuff : Set where
  mkStuff : Stuff


stuffFunc : Stuff -> (Stuff : Set) -> Stuff
stuffFunc = {!   !}
