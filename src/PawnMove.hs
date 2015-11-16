module PawnMove (
pawnPosFrom
) where

import Model

pawnPosFrom :: Position -> Piece -> Board -> [Position]
pawnPosFrom pos piece board = [(col pos, rowAdd pos 1)]

