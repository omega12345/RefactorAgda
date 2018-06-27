module ParseTree where
open import Data.String
open import Data.Nat
open import Data.Bool
open import Data.List
{-# FOREIGN GHC import ParseTree #-}

data ParseTree : Set
data TypeSignature : Set
data IdentOrLiteral : Set
data Parameter : Set
data Expr : Set
data Type : Set
data Range : Set
data Identifier : Set
data Pragma : Set
data Comment : Set


data ParseTree where
  signature : (signature : TypeSignature) -> (range : Range) -> ParseTree
  functionDefinition : (definitionOf : Identifier) -> (params : List Parameter) -> (body : Expr) -> (range : Range) -> ParseTree
  dataStructure : (dataName : Identifier) -> (parameters : List TypeSignature) -> (indexInfo : Type) -> (constructors : List TypeSignature) -> (range : Range) -> ParseTree
  pragma : (pragma : Pragma) -> (range : Range) -> ParseTree
  openImport : (opened : Bool) -> (imported : Bool) -> (moduleName : Identifier) -> (range : Range) -> ParseTree
  moduleName : (moduleName : Identifier) -> (range : Range) -> ParseTree

{-# COMPILE GHC ParseTree = data ParseTree
( Signature
| FunctionDefinition
| DataStructure
| Pragma
| OpenImport
| ModuleName
) #-}

data TypeSignature where
  typeSignature : (funcName : Identifier) -> (funcType : Type) -> (comments : List (List Comment)) -> TypeSignature

{-# COMPILE GHC TypeSignature = data TypeSignature
( TypeSignature
) #-}

data IdentOrLiteral where
  numLit : (value : ℕ) -> IdentOrLiteral
  ident : (identifier : Identifier) -> IdentOrLiteral

{-# COMPILE GHC IdentOrLiteral = data IdentOrLiteral
( NumLit
| Ident
) #-}

data Parameter where
  lit : (paramLit : IdentOrLiteral) -> Parameter
  paramApp : (paramFunc : Parameter) -> (paramArg : Parameter) -> Parameter

{-# COMPILE GHC Parameter = data Parameter
( Lit
| ParamApp
) #-}

data Expr where
  exprLit : (literal : IdentOrLiteral) -> Expr
  hole : (textInside : String) -> Expr
  functionApp : (function : Expr) -> (argument : Expr) -> Expr

{-# COMPILE GHC Expr = data Expr
( ExprLit
| Hole
| FunctionApp
) #-}

data Type where
  type : (expression : Expr) -> Type
  implicitArgument : (impArg : TypeSignature) -> Type
  explicitArgument : (expArg : TypeSignature) -> Type
  functionType : (input : Type) -> (output : Type) -> Type

{-# COMPILE GHC Type = data Type
( Type
| ImplicitArgument
| ExplicitArgument
| FunctionType
) #-}

data Range where
  range : (lastUnaffected : ℕ) -> (lastAffected : ℕ) -> Range

{-# COMPILE GHC Range = data Range
( Range
) #-}

data Identifier where
  identifier : (name : String) -> (isInRange : (ℕ -> Bool)) -> (scope : ℕ) -> (declaration : ℕ) -> Identifier

{-# COMPILE GHC Identifier = data Identifier
( Identifier
) #-}

data Pragma where
  builtin : (concept : String) -> (definition : Identifier) -> Pragma

{-# COMPILE GHC Pragma = data Pragma
( Builtin
) #-}

data Comment where
  lineComment : (content : String) -> Comment
  multiLineComment : (content : String) -> Comment

{-# COMPILE GHC Comment = data Comment
( LineComment
| MultiLineComment
) #-}
