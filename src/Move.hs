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

moves :: [Board] -> Color -> [Board]
moves boards color =
    filter (`isLegal` color) (unfilteredMoves boards color)

unfilteredMoves :: [Board] -> Color -> [Board]
unfilteredMoves boards color = [moveS square toSquare board |
    square <- filter (hasColoredP color) board,
    toSquare <- squaresFrom square boards,
    notOccupied color (position toSquare) board]
        where board = last boards

squaresFrom :: Square -> [Board] -> [Square]
squaresFrom (p,mp) b = case mp of Nothing -> []
                                  Just piece -> squaresFromP p piece b

squaresFromP :: Position -> Piece -> [Board] -> [Square]
squaresFromP position piece boards =
    filter insideBoardS $
        case piece of Piece Knight _ -> knightSquareFrom position piece board
                      Piece Pawn _ -> pawnSquareFrom position piece board
                      Piece Bishop _ -> bishopSquareFrom position piece board
                      Piece Rook _ -> rookSquareFrom position piece board
                      Piece Queen _ -> queenSquareFrom position piece board
                      Piece King _ -> kingSquareFrom position piece board
      where board = last boards

threefoldRepetition :: [Board] -> Bool
threefoldRepetition [] = False
threefoldRepetition (b:bs) = isRepeated (b:bs) b >= 3 ||
     threefoldRepetition (filter (/=b) (b:bs))

isRepeated :: [Board] -> Board -> Int
isRepeated boards board = length $ elemIndices board boards

isParalyzed :: [Board] -> Color -> Bool
isParalyzed b c = null $ moves b c

isMated :: [Board] -> Color -> Bool
isMated b c = isParalyzed b c && isInCheck b c

isInCheck :: [Board] -> Color -> Bool
isInCheck b c = any withoutKing $ unfilteredMoves b $invertC c
    where withoutKing b = not $ oneKingEach b

isLegal :: Board -> Color -> Bool
isLegal board myColor = oneKingEach board &&
    doesNotSurrenderKing board myColor

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

doesNotSurrenderKing :: Board -> Color -> Bool
doesNotSurrenderKing board myColor = all oneKingEach $ unfilteredMoves [board] $ invertC myColor

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
move from to board =
    land (fst $ pieceAt from board) to (without from board)

moveS :: Square -> Square -> Board -> Board
moveS from to board = landS (fst $ pieceAt (position from) board) to (without (position from) board)

castles :: Position -> Piece -> [Board] -> [Board]
castles pos piece boards = castlesOO pos piece boards
    -- ++ castlesOOO pos piece boards --todo

castlesOO :: Position -> Piece -> [Board] -> [Board]
castlesOO pos piece boards = [castlesBoardOO pos piece (last boards) |
    not $ isInCheck boards $ pColor piece,
    kingNotMovesThroughCheckOO pos piece boards,
    kingNotMoved piece boards,
    rookNotMoved (pColor piece) boards,
    coastClearOO pos piece (last boards)]

castlesBoardOO :: Position -> Piece -> Board -> Board
castlesBoardOO pos piece board = board--todo

kingNotMoved :: Piece -> [Board] -> Bool
kingNotMoved theKing = all (kingInStartPos (pColor theKing))
    where kingInStartPos color board = fst ( pieceAt pos board) == Just theKing
          pos = if pColor theKing == White then ('E',1) else ('E',8)

rookNotMoved :: Color -> [Board] -> Bool
rookNotMoved color boards = True --todo

coastClearOO :: Position -> Piece -> Board -> Bool
coastClearOO pos piece board = True --todo

kingNotMovesThroughCheckOO :: Position -> Piece -> [Board] -> Bool
kingNotMovesThroughCheckOO pos piece boards = True --todo
