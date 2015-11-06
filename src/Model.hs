-- Chess models
module Model (
Color(..),
Piece(..),
PType(..),
Position,
Square,
Board,
Row,
Col,
row,
col,
position,
insideBoard,
colAdd,
rowAdd,
isBlackSquare
) where

import Data.Char
import Data.List
import Data.Maybe

data Color = White | Black deriving (Eq,Show,Ord,Read,Enum,Bounded)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq,Show,Ord,Read,Enum,Bounded)
data Piece =  Piece { pType :: PType, pColor :: Color} deriving (Eq,Show,Ord,Read,Bounded)
type Position = (Char, Int)
type Square = (Position, Maybe Piece)
type Board = [Square]
type Row = [Square]
type Col = [Square]

row :: Position -> Int
row = snd

col :: Position -> Char
col pos = toUpper $ fst pos

position :: Square -> Position
position = fst

insideBoard :: Position -> Bool
insideBoard (col,row) = col `elem` ['A' .. 'H'] && row `elem` [1 .. 8]

colAdd :: Position -> Int -> Char
colAdd pos c = chr (ord (col pos) + c)

rowAdd :: Position -> Int -> Int
rowAdd pos r = row pos + r

isBlackSquare :: Position -> Bool
isBlackSquare (c,r) = even $ toIntCol c + r

toIntCol :: Char -> Int
toIntCol c = fromMaybe 0 $ elemIndex c ['A' .. 'H']
