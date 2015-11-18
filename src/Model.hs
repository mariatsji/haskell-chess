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
toIntCol,
hasOpponentOn,
notOccupied,
vacant,
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
hasColoredP color square = (snd square >>= (Just . pColor)) == Just color

squareAt :: Position -> Board -> Square
squareAt pos board = head $ filter (\s -> position s == pos) board

notOccupied :: Color -> Position -> Board -> Bool
notOccupied color pos board = not $ hasColoredP color $ squareAt pos board

vacant :: Board -> Position -> Bool
vacant board pos = insideBoard pos && isNothing (fst (pieceAt pos board))

sameColor :: Piece -> Piece -> Bool
sameColor first second = pColor first == pColor second

hasOpponentOn :: Color -> Board -> Position -> Bool
hasOpponentOn myColor board pos =
    fmap pColor (fst $ pieceAt pos board) == Just (invertC myColor)

pieceAt :: Position -> Board -> (Maybe Piece, Board)
pieceAt pos board
    | insideBoard pos = (snd $ squareAt pos board, board)
    | otherwise = error errorMsg
        where errorMsg = "should not ask for positions outside board " ++ show pos
