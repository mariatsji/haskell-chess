-- Moves and stuff

module Move (
land,
row,
col,
position,
pieceAt,
move,
moves,
unfilteredMoves,
moveWithHistory,
threefoldRepetition,
isParalyzed,
isInCheck,
isMated,
squaresFrom,
isLegal
) where

import Model
import Control.Applicative
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
unfilteredMoves board color = [moveS square toSquare board |
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

threefoldRepetition :: [Board] -> Bool
threefoldRepetition [] = False
threefoldRepetition (b:bs) = isRepeated (b:bs) b >= 3 ||
     threefoldRepetition (filter (/=b) (b:bs))

isRepeated :: [Board] -> Board -> Int
isRepeated boards board = length $ elemIndices board boards

isParalyzed :: Board -> Color -> Bool
isParalyzed b c = null $ moves b c

isMated :: Board -> Color -> Bool
isMated b c = isParalyzed b c && isInCheck b c

isInCheck :: Board -> Color -> Bool
isInCheck b c = any withoutKing $ unfilteredMoves b $invertC c
    where withoutKing b = not $ oneKingEach b

isLegal :: Board -> Color -> Bool
isLegal board myColor = oneKingEach board &&
    doesNotSurrenderKing board myColor

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

doesNotSurrenderKing :: Board -> Color -> Bool
doesNotSurrenderKing board myColor = all oneKingEach $ unfilteredMoves board $invertC myColor

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

landS :: Maybe Piece -> Square -> Board -> Board
landS piece square board = before square board ++ [square] ++ after square board
    where before x = filter isBefore
          after y = filter isAfter
          isBefore s = compS s square == LT
          isAfter s = compS s square == GT

comp :: Position -> Position -> Ordering
comp a b =  col a `compare` col b `mappend` (row a `compare` row b)

compS :: Square -> Square -> Ordering
compS a b = comp (fst a) (fst b)

without :: Position -> Board -> Board
without = land Nothing

move :: Position -> Position -> Board -> Board
move from to board = land (fst $ pieceAt from board) to (without from board)

moveS :: Square -> Square -> Board -> Board
moveS from to board = landS (fst $ pieceAt (position from) board) to (without (position from) board)

moveWithHistory :: Position -> Position -> [Board] -> [Board]
moveWithHistory from to history = history ++ [newHistory]
    where newHistory = move from to $ last history
