
module Board (
initBoard,
emptyBoard,
testBoard
) where

import Model
import Move

emptyBoard :: Board
emptyBoard = [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

testBoard = checkmateBoard

checkmateBoard = land (Just $ Piece Rook White)('A',8) stalemateBoard

stalemateBoard =
            land (Just $ Piece King White)('E',6) $
            land (Just $ Piece Pawn White)('E',7) $
            land (Just $ Piece King Black)('E',8) emptyBoard


kingBoard =
            land (Just $ Piece King White)('E',1) $
            land (Just $ Piece King Black)('E',5) emptyBoard

initBoard = land (Just $ Piece Knight Black)('G',8) $
            land (Just $ Piece Knight Black)('B',8) $
            land (Just $ Piece Bishop Black)('F',8) $
            land (Just $ Piece Bishop Black)('C',8) $
            land (Just $ Piece Rook Black)('H',8) $
            land (Just $ Piece Rook Black)('A',8) $
            land (Just $ Piece Queen Black)('D',8) $
            land (Just $ Piece King Black)('E',8) $
            land (Just $ Piece Pawn Black)('H',7) $
            land (Just $ Piece Pawn Black)('G',7) $
            land (Just $ Piece Pawn Black)('F',7) $
            land (Just $ Piece Pawn Black)('E',7) $
            land (Just $ Piece Pawn Black)('D',7) $
            land (Just $ Piece Pawn Black)('C',7) $
            land (Just $ Piece Pawn Black)('B',7) $
            land (Just $ Piece Pawn Black)('A',7) $
            land (Just $ Piece Knight White)('G',1) $
            land (Just $ Piece Knight White)('B',1) $
            land (Just $ Piece Bishop White)('F',1) $
            land (Just $ Piece Bishop White)('C',1) $
            land (Just $ Piece Rook White)('H',1) $
            land (Just $ Piece Rook White)('A',1) $
            land (Just $ Piece Queen White)('D',1) $
            land (Just $ Piece King White)('E',1) $
            land (Just $ Piece Pawn White)('H',2) $
            land (Just $ Piece Pawn White)('G',2) $
            land (Just $ Piece Pawn White)('F',2) $
            land (Just $ Piece Pawn White)('E',2) $
            land (Just $ Piece Pawn White)('D',2) $
            land (Just $ Piece Pawn White)('C',2) $
            land (Just $ Piece Pawn White)('B',2) $
            land (Just $ Piece Pawn White)('A',2) emptyBoard






