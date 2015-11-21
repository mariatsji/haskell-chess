module BishopMove (
bishopPosFrom,
bishopSquareFrom
) where

import Model
import GlideMove

bishopSquareFrom :: Position -> Piece -> Board -> [Square]
bishopSquareFrom pos piece board = (\poz -> (poz, Just piece)) <$> bishopPosFrom pos piece board

bishopPosFrom :: Position -> Piece -> Board -> [Position]
bishopPosFrom pos piece board = filter insideBoard $
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos (-1))) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos (-1)))

