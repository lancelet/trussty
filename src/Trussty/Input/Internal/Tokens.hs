module Trussty.Input.Internal.Tokens where

import           Data.Text (Text)
import           Data.Word (Word32)

-- | Position of a token in the input stream.
data Pos = Pos !LineNum !ColNum deriving (Eq, Show)
newtype LineNum = LineNum { unLineNum :: Word32 } deriving (Eq, Show)
newtype ColNum = ColNum { unColNum :: Word32 } deriving (Eq, Show)

-- | Token and its position in the input stream.
data PosToken = PosToken Token Pos deriving (Eq, Show)

-- | Tokens that appear in the input stream.
data Token
    = TokOpenBrace
    | TokCloseBrace
    | TokOpenBracket
    | TokCloseBracket
    | TokOpenParen
    | TokCloseParen
    | TokEquals
    | TokComma
    | TokArrow
    | TokIdentifier Text
    deriving (Eq, Show)
