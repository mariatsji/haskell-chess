module PawnMove (
pawnPosFrom
) where

import Model

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
