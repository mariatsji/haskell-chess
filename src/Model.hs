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
invertC,
isBlackSquare,
hasColoredP,
squareAt,
hasOpponentOn,
notOccupied,
pieceAt
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

invertC :: Color -> Color
invertC c = if c == White then Black else White

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

hasColoredP :: Color -> Square -> Bool
hasColoredP c s = case fmap (\p -> pColor p == c) (snd s) of Just True -> True
                                                             otherwise -> False

squareAt :: Position -> Board -> Square
squareAt pos board = head $ filter (\s -> position s == pos) board

notOccupied :: Color -> Position -> Board -> Bool
notOccupied color pos board = not $ hasColoredP color $ squareAt pos board

hasOpponentOn :: Color -> Board -> Position -> Bool
hasOpponentOn myColor board pos = case fmap pColor $ fst $ pieceAt pos board of
    Just opponentColor -> True
    otherwise -> False
      where opponentColor = invertC myColor

pieceAt :: Position -> Board -> (Maybe Piece, Board)
pieceAt pos board = (snd $ head $ filter rightsquare board, board)
    where rightsquare = (==) pos . fst
