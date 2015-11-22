module PawnMove (
pawnPosFrom,
pawnSquareFrom
) where

import Model
import Control.Applicative

pawnSquareFrom :: Position -> Piece -> Board -> [Square]
pawnSquareFrom pos piece board = let
    movedTo = filter (not . isPromotionSquare (pColor piece)) $ pawnMoveSquareFrom pos piece board
    promotedTo = promotedAt (pColor piece) $ pawnMoveSquareFrom pos piece board
    in movedTo ++ promotedTo

promotedAt :: Color -> [Square] -> [Square]
promotedAt color squares = [(position s, Just piece) |
    s <- squares,
    piece <- [Piece Knight color, Piece Bishop color, Piece Rook color, Piece Queen color],
    isPromotionSquare color s]

pawnMoveSquareFrom :: Position -> Piece -> Board -> [Square]
pawnMoveSquareFrom pos piece board = (\poz -> (poz, Just piece)) <$> pawnPosFrom pos piece board

pawnPosFrom :: Position -> Piece -> Board -> [Position]
pawnPosFrom pos piece board = oneMovePawn pos piece board ++
    twoMovesPawn pos piece board ++
    takesLeft pos piece board ++
    takesRight pos piece board

oneMovePawn :: Position -> Piece -> Board -> [Position]
oneMovePawn pos piece board = filter (vacant board) [oneMovePawn' pos piece board]

twoMovesPawn :: Position -> Piece -> Board -> [Position] -- should be a Maybe
twoMovesPawn pos piece board = case oneMovePawn pos piece board of --hacky fix of jumping over own pieces
    ([]) -> []
    otherwise -> filter (vacant board) (twoMovesPawn' pos piece board)

oneMovePawn' :: Position -> Piece -> Board -> Position
oneMovePawn' pos piece board
    | pColor piece == White = (col pos, rowAdd pos 1)
    | otherwise = (col pos, rowAdd pos (-1))


twoMovesPawn' :: Position -> Piece -> Board -> [Position]
twoMovesPawn' pos piece board
    | pColor piece == White && row pos == 2
        = [(col pos, rowAdd pos 2)]
    | pColor piece == Black && row pos == 8
        = [(col pos, rowAdd pos (-2))]
    | otherwise
        = []

takesLeft :: Position -> Piece -> Board -> [Position]
takesLeft pos piece board = filter
    (hasOpponentOn (pColor piece) board)
    (takesLeft' pos piece board)

takesRight :: Position -> Piece -> Board -> [Position]
takesRight pos piece board = filter
    (hasOpponentOn (pColor piece) board)
    (takesRight' pos piece board)

takesLeft' :: Position -> Piece -> Board -> [Position]
takesLeft' pos piece board
    | pColor piece == White && row pos < 8 && col pos /= 'A'
        = [(colAdd pos (-1), rowAdd pos 1)]
    | pColor piece == Black && row pos > 1 && col pos /= 'A'
        = [(colAdd pos (-1), rowAdd pos (-1))]
    | otherwise = []

takesRight' :: Position -> Piece -> Board -> [Position]
takesRight' pos piece board
    | pColor piece == White && row pos < 8 && col pos /= 'H'
        = [(colAdd pos 1, rowAdd pos 1)]
    | pColor piece == Black && row pos > 1 && col pos /= 'H'
        = [(colAdd pos 1, rowAdd pos (-1))]
    | otherwise
        = []
