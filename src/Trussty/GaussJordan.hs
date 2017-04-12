{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Trussty.GaussJordan where

import           Control.Monad.Eff
import           Control.Monad.Eff.Lift
import           Control.Monad.ST
import           Data.Proxy             (Proxy)
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as M
import           Data.Word              (Word16)

-------------------------------------------------------------------------------
-- * Basic types

type Index = Word16

newtype RowI = RowI { unRowI :: Index } deriving (Show, Eq, Ord)

newtype ColI = ColI { unColI :: Index } deriving (Show, Eq, Ord)

-- | Dense matrix with mutable elements in the ST monad.
data DSTMatrix s e = DSTMatrix
    { vmNumRows :: RowI
    , vmNumCols :: ColI
    , vmVector  :: M.STVector s e }

-- | Get an element from a DSTMatrix.
dstGet :: DSTMatrix s e -> RowI -> ColI -> ST s e
dstGet m r c = M.read (vmVector m) $ lindex ((unRowI . vmNumRows) m) r c

-- | Put an element into a DSTMatrix.
dstPut :: DSTMatrix s e -> RowI -> ColI -> e -> ST s ()
dstPut m r c = M.write (vmVector m) $ lindex ((unRowI . vmNumRows) m) r c

-- | Immutable dense matrix.
data DMatrix e = DMatrix
    { dmNumRows :: RowI
    , dmNumCols :: ColI
    , dmVector  :: V.Vector e }

-------------------------------------------------------------------------------
-- * Types for effect languages

data Matrix e x where
    NumRows :: Proxy e -> Matrix e RowI
    NumCols :: Proxy e -> Matrix e ColI
    MGet    :: RowI -> ColI -> Matrix e e
    MPut    :: RowI -> ColI -> e -> Matrix e ()

data ForIndex x where
    ForIndex :: Index -> Index -> Eff r () -> ForIndex ()

data RowManip e x where
    FindPivotRow :: Proxy e -> ColI -> RowManip e RowI
    SwapRows     :: Proxy e -> RowI -> RowI -> RowManip e ()
    SubScaled    :: RowI -> e -> RowI -> RowManip e ()

-------------------------------------------------------------------------------
-- * Effect languages

-- ** DenseMatrix language

numRows :: (Member (Matrix e) r) => Proxy e -> Eff r RowI
numRows proxy = send (NumRows proxy)

numCols :: (Member (Matrix e) r) => Proxy e -> Eff r ColI
numCols proxy = send (NumCols proxy)

mGet :: (Member (Matrix e) r) => RowI -> ColI -> Eff r e
mGet row col = send (MGet row col)

mPut :: (Member (Matrix e) r) => RowI -> ColI -> e -> Eff r ()
mPut row col element = send (MPut row col element)

-- ** ForIndex language

forIndex :: (Member ForIndex r) => Index -> Index -> Eff r () -> Eff r ()
forIndex first last action = send (ForIndex first last action)

-- ** RowManip language

findPivotRow :: (Member (RowManip e) r) => Proxy e -> ColI -> Eff r RowI
findPivotRow proxy col = send (FindPivotRow proxy col)

swapRows :: (Member (RowManip e) r) => Proxy e -> RowI -> RowI -> Eff r ()
swapRows proxy rowA rowB = send (SwapRows proxy rowA rowB)

subScaled :: (Member (RowManip e) r) => RowI -> e -> RowI -> Eff r ()
subScaled rowA scale rowB = send (SubScaled rowA scale rowB)

-------------------------------------------------------------------------------
-- * Interpreters

-- ** Interpret Matrix in terms of ST

runMatrixState :: forall r s a e.
                  ( MemberU2 Lift (Lift (ST s)) r )
               => DSTMatrix s e
               -> Eff ((Matrix e) ': r) a
               -> Eff r a
runMatrixState vm = handleRelay return go
  where
    go :: Handler (Matrix e) r a
    go (NumRows proxy)   k = numRows'         >>= k
    go (NumCols proxy)   k = numCols'         >>= k
    go (MGet row col)    k = mGet' row col    >>= k
    go (MPut row col el) k = mPut' row col el >>= k

    numRows' :: Eff r RowI
    numRows' = (return . vmNumRows) vm

    numCols' :: Eff r ColI
    numCols' = (return . vmNumCols) vm

    mGet' :: RowI -> ColI -> Eff r e
    mGet' r c = lift $ dstGet vm r c

    mPut' :: RowI -> ColI -> e -> Eff r ()
    mPut' r c el = lift $ dstPut vm r c el

-------------------------------------------------------------------------------
-- * Utility functions

w16i :: Word16 -> Int
w16i = fromIntegral . toInteger

lindex :: Word16 -> RowI -> ColI -> Int
lindex nRows r c = w16i $ (unRowI r) + (unColI c) * nRows
