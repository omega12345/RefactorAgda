{-# LANGUAGE DuplicateRecordFields #-}

module ParseTree where
import Data.Text
data ParseTree = Signature { signature :: TypeSignature
                           , range :: Range
                           }
              |  FunctionDefinition { definitionOf :: Identifier
                                    , params :: [Parameter]
                                    , body :: Expr
                                    , range :: Range
                                    }
              |  DataStructure { dataName :: Identifier
                               , parameters :: [TypeSignature]
                               , indexInfo :: Type
                               , constructors :: [TypeSignature]
                               , range :: Range
                               }
              |  Pragma { pragma :: Pragma
                        , range :: Range
                        }
              |  OpenImport { opened :: Bool
                            , imported :: Bool
                            , moduleName :: Identifier
                            , range :: Range
                            }
              |  ModuleName { moduleName :: Identifier
                            , range :: Range } deriving (Show, Eq)


data TypeSignature = TypeSignature { funcName :: Identifier
                                   , funcType :: Type
                                   , comments :: [[Comment]]
                                   } deriving (Show, Eq)

data IdentOrLiteral = NumLit {value :: Integer}
                    | Ident {identifier :: Identifier} deriving (Show, Eq)

-- parameters to a function definition are a subset of things valid
-- on the right-hand side of a definition

data Parameter = Lit {paramLit :: IdentOrLiteral}
               | ParamApp { paramFunc :: Parameter
                          , paramArg :: Parameter
                          } deriving (Show, Eq)

data Expr = ExprLit {literal :: IdentOrLiteral}
           | Hole {textInside :: Text}
           | FunctionApp { function :: Expr
                         , argument :: Expr
                         }
           deriving (Show, Eq)

data Type = Type { expression :: Expr }
            | ImplicitArgument { impArg :: TypeSignature}
            | ExplicitArgument { expArg :: TypeSignature}
            | FunctionType { input :: Type
                           , output :: Type
                           } deriving (Show, Eq)

data Range = Range { lastUnaffected :: Integer
                   , lastAffected :: Integer
                   } deriving (Show, Eq)

data Identifier = Identifier { name :: Text
                             , isInRange :: Integer -> Bool
                             , scope :: Integer
                             , declaration :: Integer
                             }

instance Eq Identifier where
  (Identifier x _ _ _) == (Identifier y _ _ _) = x == y

instance Show Identifier where
  show (Identifier x _ _ _) = show x

data Pragma = Builtin { concept :: Text
                      , definition :: Identifier
                      } deriving (Show, Eq)

data Comment = LineComment { content :: Text}
               | MultiLineComment { content :: Text } deriving (Show, Eq)
