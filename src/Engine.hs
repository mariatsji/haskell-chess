-- Moves and stuff

module Engine (
land,
row,
col,
position,
emptyBoard,
initBoard
) where

import Model
import Data.Monoid

land :: Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, Just piece)] ++ after pos board
    where before pos board = filter isBefore board
          after pos board = filter isAfter board
          comp a b =  (col a) `compare` (col b) `mappend` ((row a) `compare` (row b))
          isBefore s = comp (position s) pos == LT
          isAfter s = comp (position s) pos == GT

row :: Position -> Int
row pos = snd pos

col :: Position -> Char
col pos = fst pos

position :: Square -> Position
position s = fst s

emptyBoard :: Board
emptyBoard = [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

initBoard = land (Knight Black)('G',8) $
            land (Knight Black)('B',8) $
            land (Bishop Black)('F',8) $
            land (Bishop Black)('C',8) $
            land (Rook Black)('H',8) $
            land (Rook Black)('A',8) $
            land (Queen Black)('D',8) $
            land (King Black)('E',8) $
            land (Pawn Black)('H',7) $
            land (Pawn Black)('G',7) $
            land (Pawn Black)('F',7) $
            land (Pawn Black)('E',7) $
            land (Pawn Black)('D',7) $
            land (Pawn Black)('C',7) $
            land (Pawn Black)('B',7) $
            land (Pawn Black)('A',7) $
            land (Knight White)('G',1) $
            land (Knight White)('B',1) $
            land (Bishop White)('F',1) $
            land (Bishop White)('C',1) $
            land (Rook White)('H',1) $
            land (Rook White)('A',1) $
            land (Queen White)('D',1) $
            land (King White)('E',1) $
            land (Pawn White)('H',2) $
            land (Pawn White)('G',2) $
            land (Pawn White)('F',2) $
            land (Pawn White)('E',2) $
            land (Pawn White)('D',2) $
            land (Pawn White)('C',2) $
            land (Pawn White)('B',2) $
            land (Pawn White)('A',2) emptyBoard




