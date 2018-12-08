{-# LANGUAGE DuplicateRecordFields #-}

module ParseTree where
import Data.Text
data ParseTree = Signature { signature :: TypeSignature
                           , range :: Range
                           }
              |  FunctionDefinition { definitionOf :: Identifier
                                    , params :: [Expr]
                                    , body :: Expr
                                    , range :: Range
                                    }
              |  DataStructure { dataName :: Identifier
                               , parameters :: [TypeSignature]
                               , indexInfo :: Expr
                               , constructors :: [TypeSignature]
                               , range :: Range
                               , comments :: [[Comment]] -- implicit
                               }
              |  Pragma { pragma :: Pragma
                        , range :: Range
                        }
              |  OpenImport { opened :: Bool
                            , imported :: Bool
                            , moduleName :: Identifier
                            , range :: Range
                            , comments :: [[Comment]] -- implicit
                            }
              |  ModuleName { moduleName :: Identifier
                            , range :: Range } deriving (Show, Eq)


data TypeSignature = TypeSignature { funcName :: Identifier
                                   , funcType :: Expr
                                   } deriving (Show, Eq)

data Expr = NumLit {value :: Integer -- implicit
                   , position :: Range -- implicit
                   , commentsBef :: [Comment] -- implicit
                   , commentsAf :: [Comment] -- implicit
                   }
           | Ident {identifier :: Identifier
                    }
           | Hole {textInside :: Text -- implicit
                  , position :: Range -- implicit
                  , commentsBef :: [Comment] -- implicit
                  , commentsAf :: [Comment] -- implicit
                  }
           | FunctionApp { firstPart :: Expr
                         , secondPart :: Expr
                         , isType :: Bool -- implicit
                         }
           | Implicit {expr :: Expr}
           | Underscore {position :: Range -- implicit
                        , commentsBef :: [Comment] -- implicit
                        , commentsAf :: [Comment] -- implicit
                        }
           | NamedArgument { arg :: TypeSignature
                           , explicit :: Bool -- implicit
                           }
           deriving (Show, Eq)

data Range = Range { lastUnaffected :: Integer
                   , lastAffected :: Integer
                   } deriving (Show, Eq)

data Identifier = Identifier { name :: Text
                             , isInRange :: Integer -> RangePosition
                             , scope :: Integer
                             , declaration :: Integer
                             , inScope :: Bool -- implicit
                             , commentsBefore :: [Comment] -- implicit
                             , commentsAfter :: [Comment] -- implicit
                             }

data RangePosition = Before {} | Inside {} | After {}

instance Eq Identifier where
  (Identifier x _ _ _ _ _ _) == (Identifier y _ _ _ _ _ _) = x == y

instance Show Identifier where
  show (Identifier x _ y z a b c) = show x

data Pragma = Builtin { concept :: Text
                      , definition :: Identifier
                      }
              | Option { opts :: [Text]
                       } deriving (Show, Eq)

data Comment = Comment { content :: Text  -- implicit
                       , codePos :: Range -- implicit
                       , isMultiLine :: Bool -- implicit
                       } deriving (Show, Eq)
