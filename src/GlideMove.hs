
module GlideMove (
glides
) where

import Model

glides :: Position -> Piece -> Board -> (Position -> Position) -> [Position]
glides posFrom piece board next
    | insideBoard (next posFrom) && vacant board (next posFrom)
        = next posFrom : glides (next posFrom) piece board next
    | insideBoard (next posFrom) && hasOpponentOn (pColor piece) board (next posFrom)
        = [next posFrom]
    | otherwise
        = []


