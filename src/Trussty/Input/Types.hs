module Trussty.Input.Types where

import Data.Map (Map)
import Data.Text (Text)

newtype ProcedureName = ProcedureName Text
                      deriving (Eq, Show)

newtype VariableName = VariableName Text
                     deriving (Eq, Show)

newtype MapKey = MapKey Text
               deriving (Eq, Show)
    
newtype InMap = InMap (Map MapKey Expr)
              deriving (Eq, Show)

data Expr
    = ExprMap InMap
    | ExprList [Expr]
    | ExprTuple [Expr]
    deriving (Eq, Show)

data Statement
    = Assignment VariableName Expr
    | Procedure ProcedureName InMap
