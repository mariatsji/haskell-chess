-- Chess models
module Model (
Color(..),
Piece(..),
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
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq,Show,Ord,Read)
type Position = (Char, Int)
type Square = (Position, Maybe Piece)
type Board = [Square]
type Row = [Square]
type Col = [Square]

row :: Position -> Int
row pos = snd pos

col :: Position -> Char
col pos = toUpper $ fst pos

position :: Square -> Position
position s = fst s