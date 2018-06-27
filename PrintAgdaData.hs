{-# LANGUAGE OverloadedStrings #-}
module PrintAgdaData where
import ParseHaskellData
import Data.Text hiding (map, intersperse)
import Data.List
toAgda :: [DataStructure] -> Text
toAgda x = Data.Text.concat
            ["\n"
            ,Data.Text.concat $ map dataStructureToAgdaDeclaration x
            ,"\n"
            ,Data.Text.concat $ map dataStructureToAgda x
            ]

dataStructureToAgda :: DataStructure -> Text
dataStructureToAgda (NameAndAllConstructors dataname constructors) =
  "\ndata " `append` dataname `append` " where\n"
  `append` Data.Text.concat ( map (constructorToAgda dataname) constructors) `append`
  "\n{-# COMPILE GHC " `append` dataname `append` " = data " `append` dataname `append` "\n( " `append`
  (Data.Text.concat . intersperse "\n| " $ map consname constructors) `append`
  "\n) #-}\n"

constructorToAgda :: Text -> Constructor -> Text
constructorToAgda dataname (Constructor name contents) =
  "  " `append` toLower (singleton $ Data.Text.head name) `append` Data.Text.tail name `append`
  " : " `append`
  -- contents is a list of (varname, typesList)
  Data.Text.concat ( map varToAgda contents) `append`
  dataname `append`
  "\n"

varToAgda :: (Text, [Text]) -> Text
varToAgda (name, types) = addArrow $ Data.Text.concat ["(", name, " : ", typeToAgda types, ")"]

addArrow :: Text -> Text
addArrow input = append input " -> "

typeToAgda :: [Text] -> Text
typeToAgda [x] | x == "Text" = "String"
               | x == "Boolean" = "Bool"
               | x == "Integer" = "ℕ"
               | Data.Text.head x == '['
                        = if Data.Text.head (Data.Text.tail x) == '['
                          then "List (" `append` recursiveCall x
                                          `append` ")"
                          else "List " `append` recursiveCall x
                 | otherwise = x
      where recursiveCall x = typeToAgda [Data.Text.init $ Data.Text.tail x]
typeToAgda (x : xs) = "(" `append` addArrow (typeToAgda [x]) `append` typeToAgda xs `append` ")"

dataStructureToAgdaDeclaration :: DataStructure -> Text
dataStructureToAgdaDeclaration (NameAndAllConstructors dataname constructors) =
  "data " `append` dataname `append` " : Set\n"
