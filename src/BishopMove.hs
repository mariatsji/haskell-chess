module BishopMove (
bishopPosFrom
) where

import Model
import GlideMove

bishopPosFrom :: Position -> Piece -> Board -> [Position]
bishopPosFrom pos piece board = filter insideBoard $
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos (-1))) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos (-1)))

