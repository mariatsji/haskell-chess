-- Chess models
module Model (
Color(..),
Piece(..),
PType(..),
Position,
Square,
Board,
Row,
Col,
row,
col,
position
) where

import Data.Char

data Color = White | Black deriving (Eq,Show,Ord,Read,Enum,Bounded)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq,Show,Ord,Read,Enum,Bounded)
data Piece =  Piece { pType :: PType, pColor :: Color} deriving (Eq,Show,Ord,Read,Bounded)
type Position = (Char, Int)
type Square = (Position, Maybe Piece)
type Board = [Square]
type Row = [Square]
type Col = [Square]

row :: Position -> Int
row = snd

col :: Position -> Char
col pos = toUpper $ fst pos

position :: Square -> Position
position = fst

