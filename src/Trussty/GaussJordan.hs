{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Trussty.GaussJordan where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Word                    (Word16)

-------------------------------------------------------------------------------
-- * Basic types

type Index = Word16
    
newtype RowIndex = RowIndex { unRowIndex :: Index } deriving (Show, Eq, Ord)

newtype ColIndex = ColIndex { unColIndex :: Index } deriving (Show, Eq, Ord)

