module BishopMove (
bishopPosFrom
) where

import Model

bishopPosFrom :: Position -> Piece -> Board -> [Position]
bishopPosFrom pos piece board = []

glides :: Position -> [Position]
glides pos = [(colAdd pos x, rowAdd pos y) |
    x <- [-8 .. 8],
    y <- [-8 .. 8],
    abs x == abs y]

unHinderedGlides :: Position -> Piece -> Board -> [Position]
unHinderedGlides pos piece board = glides pos
