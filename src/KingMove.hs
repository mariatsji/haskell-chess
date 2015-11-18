module KingMove (
kingPosFrom
) where

import Data.Char
import QueenMove
import Model

kingPosFrom :: Position -> Piece -> Board -> [Position]
kingPosFrom pos piece board = filter (\qp -> distance pos qp == 1) $ queenPosFrom pos piece board

distance ::Position -> Position -> Int
distance fromPos toPos = max (rowDistance fromPos toPos) (colDistance fromPos toPos)
    where rowDistance f t = row t - row f
          colDistance f t = toIntCol (col t) - toIntCol (col f)
