
module KnightMove (
knightPosFrom
) where

import Model

knightPosFrom :: Position -> Piece -> Board -> [Position]
knightPosFrom pos piece board = [(newCol,newRow) |
    a <- [-1,1,-2,2],
    b <- [-1,1,-2,2],
    let (newCol,newRow) = (colAdd pos a, rowAdd pos b),
    abs a /= abs b]

