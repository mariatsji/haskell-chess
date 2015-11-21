module QueenMove (
queenPosFrom,
queenSquareFrom
) where

import BishopMove
import RookMove
import Model

queenSquareFrom :: Position -> Piece -> Board -> [Square]
queenSquareFrom pos piece board = (\poz -> (poz, Just piece)) <$> queenPosFrom pos piece board

queenPosFrom :: Position -> Piece -> Board -> [Position]
queenPosFrom pos piece board = bishopPosFrom pos piece board ++ rookPosFrom pos piece board
