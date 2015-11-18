module BishopMove (
bishopPosFrom
) where

import Model

bishopPosFrom :: Position -> Piece -> Board -> [Position]
bishopPosFrom pos piece board = filter insideBoard $
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos 1, rowAdd pos (-1))) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos 1)) ++
    glides pos piece board (\pos -> (colAdd pos (-1), rowAdd pos (-1)))

glides :: Position -> Piece -> Board -> (Position -> Position) -> [Position]
glides posFrom piece board next
    | insideBoard (next posFrom) && vacant board (next posFrom)
        = next posFrom : glides (next posFrom) piece board next
    | insideBoard (next posFrom) && hasOpponentOn (pColor piece) board (next posFrom)
        = [next posFrom]
    | otherwise
        = []
