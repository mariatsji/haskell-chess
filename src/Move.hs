-- Moves and stuff

module Move (
land,
row,
col,
position,
emptyBoard,
initBoard,
pieceAt,
move,
moveWithHistory,
isLegal
) where

import Model
import Data.Monoid
import Data.List

isLegal :: [Board] -> Bool
isLegal [] = False
isLegal (b:bs) = oneKingEach b --todo

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (King White) b) &&
    not (null $ findPiece (King Black) b)

findPiece :: Piece -> Board -> [Square]
findPiece piece = filter (`hasPiece` piece)

hasPiece :: Square -> Piece -> Bool
hasPiece s p = snd s == Just p

land :: Maybe Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, piece)] ++ after pos board
    where before s = filter isBefore
          after s = filter isAfter
          isBefore s = comp (position s) pos == LT
          isAfter s = comp (position s) pos == GT

comp :: Position -> Position -> Ordering
comp a b =  col a `compare` col b `mappend` (row a `compare` row b)

comp' :: Square -> Square -> Ordering
comp' a b = comp (position a) (position b)

without :: Position -> Board -> Board
without = land Nothing

move :: Position -> Position -> Board -> Board
move from to board = land (fst $ pieceAt from board) to (without from board)

moveWithHistory :: Position -> Position -> [Board] -> [Board]
moveWithHistory from to history = history ++ [newHistory]
    where newHistory = move from to $ last history

pieceAt :: Position -> Board -> (Maybe Piece, Board)
pieceAt pos board = (snd $ head $ filter rightsquare board, board)
    where rightsquare = (==) pos . fst

sort :: Board -> Board
sort = sortBy comp'

emptyBoard :: Board
emptyBoard = Move.sort [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

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




