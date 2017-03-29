module Trussty.SparseSolver where

import           Control.Monad.ST        (ST)
import qualified Data.HashTable.ST.Basic as H
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Data.Vector             as V
import           Data.Vector.Mutable     (STVector)
import qualified Data.Vector.Mutable     as M
import qualified Data.Vector.Unboxed     as U

data SparseElem
    = Elem Double
    | ElemZero

data SparseMatrix = SparseMatrix
    { smNRows :: Int
    , smNCols :: Int
    , smElem  :: Int -> Int -> SparseElem
    }

type HashTable s k v = H.HashTable s k v

data SolveState s = SolveState
    { ssNRows      :: Int
    , ssNCols      :: Int
    , ssColRem     :: Int
    , ssElem       :: HashTable s (Int, Int) Double
    , ssRowMapping :: STVector s Int
    }

