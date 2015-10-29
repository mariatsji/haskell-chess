-- Chess models
module Model (
Color(..),
Piece(..),
Position,
Square,
Board,
emptyBoard
) where

data Color = White | Black deriving (Eq,Show,Ord,Read,Enum,Bounded)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq,Show,Ord,Read)
type Position = (Char, Int)
type Square = (Position, Maybe Piece)
type Board = [Square]

emptyBoard :: Board
emptyBoard = [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]



