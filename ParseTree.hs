{-# LANGUAGE DuplicateRecordFields #-}

module ParseTree where
import Data.Text
data ParseTree = Signature { signature :: TypeSignature
                           , range :: Range
                           }
              |  FunctionDefinition { definitionOf :: Text
                                    , params :: [Expr]
                                    , body :: Expr
                                    , range :: Range
                                    }
              |  DataStructure { dataName :: Text
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
                            , moduleName :: [Text]
                            , range :: Range
                            }
              |  ModuleName { moduleName :: [Text]
                            , range :: Range } deriving Show


data TypeSignature = TypeSignature { funcName :: Text
                                   , funcType :: Type
                                   , comments :: [[Comment]]
                                   } deriving Show

data Expr = Ident {name :: Text}
          | NumLit {value :: Integer}
          | Hole {textInside :: Text}
          | FunctionApp { function :: Expr
                        , argument :: Expr
                        } deriving Show

data Type = Type { expression :: Expr }
            | ImplicitArgument { impArg :: TypeSignature}
            | ExplicitArgument { expArg :: TypeSignature}
            | FunctionType { input :: Type
                           , output :: Type
                           } deriving Show

data Range = Range { lastUnaffected :: Integer
                   , lastAffected :: Integer
                   , needsUpdating :: Bool
                   } deriving Show

data Pragma = Builtin { concept :: Text
                      , definition :: Expr
                      } deriving Show

data Comment = LineComment { content :: Text}
               | MultiLineComment { content :: Text } deriving Show
