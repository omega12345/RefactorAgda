module ParseTree where
open import Data.String
open import Data.Nat
open import Data.Bool
open import Data.List
{-# FOREIGN GHC import ParseTree #-}

data ParseTree : Set
data TypeSignature : Set
data Expr : Set
data Type : Set
data Range : Set
data Identifier : Set
data RangePosition : Set
data Pragma : Set
data Comment : Set


data ParseTree where
  signature : (signature : TypeSignature) -> (range : Range) -> ParseTree
  functionDefinition : (definitionOf : Identifier) -> (params : List Expr) -> (body : Expr) -> (range : Range) -> ParseTree
  dataStructure : (dataName : Identifier) -> (parameters : List TypeSignature) -> (indexInfo : Type) -> (constructors : List TypeSignature) -> (range : Range) -> {comments : List (List Comment)} -> ParseTree
  pragma : (pragma : Pragma) -> (range : Range) -> ParseTree
  openImport : (opened : Bool) -> (imported : Bool) -> (moduleName : Identifier) -> (range : Range) -> {comments : List (List Comment)} -> ParseTree
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
  typeSignature : (funcName : Identifier) -> (funcType : Type) -> TypeSignature

{-# COMPILE GHC TypeSignature = data TypeSignature
( TypeSignature
) #-}

data Expr where
  numLit : {value : ℕ} -> {position : Range} -> {commentsBef : List Comment} -> {commentsAf : List Comment} -> Expr
  ident : (identifier : Identifier) -> Expr
  hole : {textInside : String} -> {position : Range} -> {commentsBef : List Comment} -> {commentsAf : List Comment} -> Expr
  functionApp : (function : Expr) -> (argument : Expr) -> Expr

{-# COMPILE GHC Expr = data Expr
( NumLit
| Ident
| Hole
| FunctionApp
) #-}

data Type where
  type : (expression : Expr) -> Type
  namedArgument : (arg : TypeSignature) -> {explicit : Bool} -> Type
  functionType : (input : Type) -> (output : Type) -> Type

{-# COMPILE GHC Type = data Type
( Type
| NamedArgument
| FunctionType
) #-}

data Range where
  range : (lastUnaffected : ℕ) -> (lastAffected : ℕ) -> Range

{-# COMPILE GHC Range = data Range
( Range
) #-}

data Identifier where
  identifier : (name : String) -> (isInRange : (ℕ -> RangePosition)) -> (scope : ℕ) -> (declaration : ℕ) -> {commentsBefore : List Comment} -> {commentsAfter : List Comment} -> Identifier

{-# COMPILE GHC Identifier = data Identifier
( Identifier
) #-}

data RangePosition where
  before : RangePosition
  inside : RangePosition
  after : RangePosition

{-# COMPILE GHC RangePosition = data RangePosition
( Before
| Inside
| After
) #-}

data Pragma where
  builtin : (concept : String) -> (definition : Identifier) -> Pragma
  option : (opts : List String) -> Pragma

{-# COMPILE GHC Pragma = data Pragma
( Builtin
| Option
) #-}

data Comment where
  comment : {content : String} -> {codePos : Range} -> {isMultiLine : Bool} -> Comment

{-# COMPILE GHC Comment = data Comment
( Comment
) #-}
