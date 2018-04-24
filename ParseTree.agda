module ParseTree where
open import Data.String
open import Data.Nat
open import Data.Bool
open import Data.List
{-# FOREIGN GHC import ParseTree #-}
{-
data Comment : Set where
 lineComment : String -> Comment
 multiLineComment : String -> Comment

{-# COMPILE GHC Comment = data Comment
( LineComment
| MultiLineComment
) #-}

getContent : Comment -> String
getContent (lineComment x) = x
getContent (multiLineComment x) = x -}

data ParseTree : Set
data TypeSignature : Set
data Expr : Set
data Type : Set
data Range : Set
data Pragma : Set
data Comment : Set


data ParseTree where
  signature : TypeSignature -> Range -> ParseTree
  functionDefinition : String -> List Expr -> Expr -> Range -> ParseTree
  dataStructure : String -> List TypeSignature -> Type -> List TypeSignature -> Range -> ParseTree
  pragma : Pragma -> Range -> ParseTree

{-# COMPILE GHC ParseTree = data ParseTree
( Signature
| FunctionDefinition
| DataStructure
| Pragma
) #-}

data TypeSignature where
  typeSignature : String -> Type -> List (List Comment) -> TypeSignature

{-# COMPILE GHC TypeSignature = data TypeSignature
( TypeSignature
) #-}

data Expr where
  ident : String -> Expr
  numLit : ℕ -> Expr
  hole : String -> Expr
  functionApp : Expr -> Expr -> Expr

{-# COMPILE GHC Expr = data Expr
( Ident
| NumLit
| Hole
| FunctionApp
) #-}

data Type where
  type : Expr -> Type
  implicitArgument : TypeSignature -> Type
  explicitArgument : TypeSignature -> Type
  functionType : Type -> Type -> Type

{-# COMPILE GHC Type = data Type
( Type
| ImplicitArgument
| ExplicitArgument
| FunctionType
) #-}

data Range where
  range : ℕ -> ℕ -> Bool -> Range

{-# COMPILE GHC Range = data Range
( Range
) #-}

data Pragma where
  builtin : String -> Expr -> Pragma

{-# COMPILE GHC Pragma = data Pragma
( Builtin
) #-}

data Comment where
  lineComment : String -> Comment
  multiLineComment : String -> Comment

{-# COMPILE GHC Comment = data Comment
( LineComment
| MultiLineComment
) #-}
