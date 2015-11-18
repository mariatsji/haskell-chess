module QueenMove (
queenPosFrom
) where

import BishopMove
import RookMove
import Model

queenPosFrom :: Position -> Piece -> Board -> [Position]
queenPosFrom pos piece board = bishopPosFrom pos piece board ++ rookPosFrom pos piece board
