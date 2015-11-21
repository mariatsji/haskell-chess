module RookMove (
rookPosFrom,
rookSquareFrom
) where

import Model
import GlideMove

rookSquareFrom :: Position -> Piece -> Board -> [Square]
rookSquareFrom pos piece board = (\poz -> (poz, Just piece)) <$> rookPosFrom pos piece board

rookPosFrom :: Position -> Piece -> Board -> [Position]
rookPosFrom pos piece board = filter insideBoard $
    glides pos piece board (\pos -> (colAdd pos 0, rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos 0, rowAdd pos (-1))) ++
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos 0)) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos 0))
