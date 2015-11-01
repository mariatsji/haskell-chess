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

land :: Maybe Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, piece)] ++ after pos board
    where before pos board = filter isBefore board
          after pos board = filter isAfter board
          comp a b =  (col a) `compare` (col b) `mappend` ((row a) `compare` (row b))
          isBefore s = comp (position s) pos == LT
          isAfter s = comp (position s) pos == GT

without :: Position -> Board -> Board
without pos board = land Nothing pos board

move :: Position -> Position -> Board -> Board
move from to board = land (pieceAt from board) to (without from board)

pieceAt :: Position -> Board -> Maybe Piece
pieceAt pos board = Nothing --todo

row :: Position -> Int
row pos = snd pos

col :: Position -> Char
col pos = fst pos

position :: Square -> Position
position s = fst s

emptyBoard :: Board
emptyBoard = [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

initBoard = land (Just $ Knight Black)('G',8) $
            land (Just $ Knight Black)('B',8) $
            land (Just $ Bishop Black)('F',8) $
            land (Just $ Bishop Black)('C',8) $
            land (Just $ Rook Black)('H',8) $
            land (Just $ Rook Black)('A',8) $
            land (Just $ Queen Black)('D',8) $
            land (Just $ King Black)('E',8) $
            land (Just $ Pawn Black)('H',7) $
            land (Just $ Pawn Black)('G',7) $
            land (Just $ Pawn Black)('F',7) $
            land (Just $ Pawn Black)('E',7) $
            land (Just $ Pawn Black)('D',7) $
            land (Just $ Pawn Black)('C',7) $
            land (Just $ Pawn Black)('B',7) $
            land (Just $ Pawn Black)('A',7) $
            land (Just $ Knight White)('G',1) $
            land (Just $ Knight White)('B',1) $
            land (Just $ Bishop White)('F',1) $
            land (Just $ Bishop White)('C',1) $
            land (Just $ Rook White)('H',1) $
            land (Just $ Rook White)('A',1) $
            land (Just $ Queen White)('D',1) $
            land (Just $ King White)('E',1) $
            land (Just $ Pawn White)('H',2) $
            land (Just $ Pawn White)('G',2) $
            land (Just $ Pawn White)('F',2) $
            land (Just $ Pawn White)('E',2) $
            land (Just $ Pawn White)('D',2) $
            land (Just $ Pawn White)('C',2) $
            land (Just $ Pawn White)('B',2) $
            land (Just $ Pawn White)('A',2) emptyBoard




