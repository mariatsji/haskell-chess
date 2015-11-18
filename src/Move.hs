-- Moves and stuff

module Move (
land,
row,
col,
position,
pieceAt,
move,
moves,
moveWithHistory,
positionsFrom,
isLegal
) where

import Model
import Data.Monoid
import Data.List
import Data.Char
import Data.Maybe
import KnightMove
import PawnMove
import BishopMove
import RookMove
import QueenMove
import KingMove

moves :: Board -> Color -> [Board]
moves board color = [move (position square) toPos board |
    square <- filter (hasColoredP color) board,
    toPos <- positionsFrom square board,
    isLegal board,
    notOccupied color toPos board]

positionsFrom :: Square -> Board -> [Position]
positionsFrom (p,mp) b = case mp of Nothing -> []
                                    Just piece -> positionsFrom' p piece b

positionsFrom' :: Position -> Piece -> Board -> [Position]
positionsFrom' position piece board =
    filter insideBoard $ case piece of Piece Knight _ -> knightPosFrom position piece board
                                       Piece Pawn _ -> pawnPosFrom position piece board
                                       Piece Bishop _ -> bishopPosFrom position piece board
                                       Piece Rook _ -> rookPosFrom position piece board
                                       Piece Queen _ -> queenPosFrom position piece board
                                       Piece King _ -> kingPosFrom position piece board

isLegal :: Board -> Bool
isLegal = oneKingEach

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

findPiece :: Piece -> Board -> [Square]
findPiece piece = filter (`hasPiece` piece)

hasPiece :: Square -> Piece -> Bool
hasPiece s p = snd s == Just p

land :: Maybe Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, piece)] ++ after pos board
    where before x = filter isBefore
          after y = filter isAfter
          isBefore s = comp (position s) pos == LT
          isAfter s = comp (position s) pos == GT

comp :: Position -> Position -> Ordering
comp a b =  col a `compare` col b `mappend` (row a `compare` row b)

without :: Position -> Board -> Board
without = land Nothing

move :: Position -> Position -> Board -> Board
move from to board = land (fst $ pieceAt from board) to (without from board)

moveWithHistory :: Position -> Position -> [Board] -> [Board]
moveWithHistory from to history = history ++ [newHistory]
    where newHistory = move from to $ last history
