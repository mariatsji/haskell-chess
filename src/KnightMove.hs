
module KnightMove (
knightSquareFrom,
knightPosFrom
) where

import Model

knightSquareFrom :: Position -> Piece -> Board -> [Square]
knightSquareFrom pos piece board = (\poz -> (poz, Just piece)) <$> knightPosFrom pos piece board

knightPosFrom :: Position -> Piece -> Board -> [Position]
knightPosFrom pos piece board = [(newCol,newRow) |
    a <- [-1,1,-2,2],
    b <- [-1,1,-2,2],
    let (newCol,newRow) = (colAdd pos a, rowAdd pos b),
    abs a /= abs b]

