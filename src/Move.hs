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
squaresFrom,
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
moves board color = filter (`isLegal` color) $ unfilteredMoves board color

unfilteredMoves :: Board -> Color -> [Board]
unfilteredMoves board color = [move (position square) (position toSquare) board |
    square <- filter (hasColoredP color) board,
    toSquare <- squaresFrom square board,
    notOccupied color (position toSquare) board]

squaresFrom :: Square -> Board -> [Square]
squaresFrom (p,mp) b = case mp of Nothing -> []
                                  Just piece -> squaresFrom' p piece b

squaresFrom' :: Position -> Piece -> Board -> [Square]
squaresFrom' position piece board =
    filter insideBoardS $ case piece of Piece Knight _ -> knightSquareFrom position piece board
                                        Piece Pawn _ -> pawnSquareFrom position piece board
                                        Piece Bishop _ -> bishopSquareFrom position piece board
                                        Piece Rook _ -> rookSquareFrom position piece board
                                        Piece Queen _ -> queenSquareFrom position piece board
                                        Piece King _ -> kingSquareFrom position piece board

isLegal :: Board -> Color -> Bool
isLegal board myColor = oneKingEach board &&
    doesNotSurrenderKing board myColor

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

doesNotSurrenderKing :: Board -> Color -> Bool
doesNotSurrenderKing board myColor = not $ any withoutKing $ unfilteredMoves board $invertC myColor
    where withoutKing b = not $ oneKingEach b

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
