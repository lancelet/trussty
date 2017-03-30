module Trussty.Input.Internal.Lexer where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString                  (ByteString)
import           Data.List                        (scanl')
import           Data.Maybe                       (mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T (decodeUtf8)
import           Data.Word                        (Word32)
import           Trussty.Input.Internal.Tokens

{-
Interesting approach which annotates each character with its source
position:
  https://github.com/ambiata/icicle/blob/master/icicle-source/src/Icicle/Source/Lexer/Lexer.hs
-}

-- | Possible errors that can occur when lexing.
data LexError
    = LexErrorUnknownCharacter !Char

-- | Performs lexing on the input stream.
lexer :: ByteString -> Either LexError [PosToken]
lexer bs = case lexraw bs of
    Left err -> Left err
    Right rtl -> Right (mkToken <$> goodTokens)
      where
        positions = scanPos (tokenDelta <$> rtl)
        maybeToken (rt, pos) = case rt of
            RawToken tok -> Just (tok, pos)
            Junk _       -> Nothing
        goodTokens = mapMaybe maybeToken (zip rtl positions)
        mkToken (tok, pos) = PosToken tok pos
    
-- | Scans over a list of 'DeltaPos' values accumulating a list of 'Pos' values.
scanPos :: [DeltaPos] -> [Pos]
scanPos = scanl' updatePos (Pos (LineNum 0) (ColNum 0))

lexraw :: ByteString -> Either LexError [RawToken]
lexraw = undefined

-- | Change in position.
data DeltaPos
    -- | Change in column ONLY (no newline).
    = DeltaPosCols DeltaCol
    -- | Change in lines, and an remaining offset in columns.
    | DeltaPosLines DeltaLine ColNum deriving (Eq, Show)

newtype DeltaLine = DeltaLine { unDeltaLine :: Word32 } deriving (Eq, Show)
newtype DeltaCol = DeltaCol { unDeltaCol :: Word32 } deriving (Eq, Show)

-- | Updates position in the stream using a delta.
updatePos :: Pos -> DeltaPos -> Pos
updatePos (Pos l c) (DeltaPosCols dc) = Pos l c'
  where
    c' = ColNum (unColNum c + unDeltaCol dc)
updatePos (Pos l c) (DeltaPosLines dl c') = Pos l' c'
  where
    l' = LineNum (unLineNum l + unDeltaLine dl)

-- | Raw tokens in the stream.
data RawToken
    = RawToken Token
    | Junk ByteString
    deriving (Eq, Show)

-- | Compute the offset caused by a single token.
tokenDelta :: RawToken -> DeltaPos
tokenDelta (RawToken t) = case t of
    (TokIdentifier value) -> DeltaPosCols (DeltaCol (iw32 (T.length value)))
    _                     -> DeltaPosCols (DeltaCol 1)
tokenDelta (Junk bs) = case txtLines of
    [line]  -> DeltaPosCols (DeltaCol (iw32 (T.length line)))
    last:ls -> DeltaPosLines
                       (DeltaLine (iw32 (length ls)))
                       (ColNum (iw32 (T.length last)))
  where
    txtLines = reverse (T.lines (T.decodeUtf8 bs))

-- | Converts an Int to a Word32. (Assumes it's in range.)
iw32 :: Int -> Word32
iw32 = fromInteger . toInteger
