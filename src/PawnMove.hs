module PawnMove (
pawnPosFrom
) where

import Model

pawnPosFrom :: Position -> Piece -> Board -> [Position]
pawnPosFrom pos piece board
    | pColor piece == White = (col pos, rowAdd pos 1) : twoMovesPawn pos piece board
    | pColor piece == Black = (col pos, rowAdd pos (-1)) : twoMovesPawn pos piece board

twoMovesPawn :: Position -> Piece -> Board -> [Position]
twoMovesPawn pos piece board
    | pColor piece == White && row pos == 2 = [(col pos, rowAdd pos 2)]
    | pColor piece == Black && row pos == 8 = [(col pos, rowAdd pos (-2))]
    | otherwise = []
