module BishopMove (
bishopPosFrom
) where

import Model

bishopPosFrom :: Position -> Piece -> Board -> [Position]
bishopPosFrom pos piece board = []

up :: Position -> Position -> Piece -> Board -> [Position]
up posFrom posTo piece board
    | vacant board posTo = posTo : up posFrom (nextUp posFrom) piece board
    | hasOpponentOn (pColor piece) board posTo =  [posTo]
    | otherwise = []

nextUp :: Position -> Position
nextUp pos = (colAdd pos 1, rowAdd pos 1)
