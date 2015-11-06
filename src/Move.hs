-- Moves and stuff

module Move (
land,
row,
col,
position,
pieceAt,
move,
moveWithHistory,
isLegal
) where

import Model
import Data.Monoid
import Data.List
import Data.Char
import KnightMove

positionsFrom :: Square -> Board -> [Position]
positionsFrom (position, Just (Piece Knight _)) = knightPosFrom position

notOwnPieceOn :: Position -> Color -> Board -> Bool
notOwnPieceOn pos color board = False -- todo

isLegal :: [Board] -> Bool
isLegal [] = False
isLegal (b:bs) = oneKingEach b

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

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

