module KingMove (
kingPosFrom,
kingSquareFrom
) where

import Data.Char
import QueenMove
import Model

kingSquareFrom :: Position -> Piece -> Board -> [Square]
kingSquareFrom pos piece board = (\poz -> (poz, Just piece)) <$> kingPosFrom pos piece board

kingPosFrom :: Position -> Piece -> Board -> [Position]
kingPosFrom pos piece board = filter (\qp -> distance pos qp == 1) $ queenPosFrom pos piece board

distance ::Position -> Position -> Int
distance fromPos toPos = max (rowDistance fromPos toPos) (colDistance fromPos toPos)
    where rowDistance f t = abs (row t - row f)
          colDistance f t = abs (toIntCol (col t) - toIntCol (col f))

