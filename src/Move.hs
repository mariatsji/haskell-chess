-- Moves and stuff

module Move (
land,
row,
col,
position,
emptyBoard,
initBoard,
pieceAt,
move,
moveWithHistory,
isLegal,
knightPosFrom
) where

import Model
import Data.Monoid
import Data.List
import Data.Char

positionsFrom :: Square -> Board -> [Position]
positionsFrom (position, Just (Piece Knight _)) board = knightPosFrom position board

knightPosFrom :: Position -> Board -> [Position]
knightPosFrom pos board = [(newCol,newRow) |
    a <- [-1,1,-2,2],
    b <- [-1,1,-2,2],
    let (newCol,newRow) = (colAdd pos a, rowAdd pos b),
    abs a /= abs b,
    insideBoard (newCol,newRow)]

insideBoard :: Position -> Bool
insideBoard (col,row) = col `elem` ['A' .. 'H'] && row `elem` [1 .. 8]

notOwnPieceOn :: Position -> Color -> Board -> Bool
notOwnPieceOn pos color board = False -- todo

colAdd :: Position -> Int -> Char
colAdd pos c = chr (ord (col pos) + c)

rowAdd :: Position -> Int -> Int
rowAdd pos r = (row pos) + r

isLegal :: [Board] -> Bool
isLegal [] = False
isLegal (b:bs) = oneKingEach b

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

findPiece :: Piece -> Board -> [Square]
findPiece piece = filter (`hasPiece` piece)

hasPiece :: Square -> Piece -> Bool
hasPiece s p = snd s == Just p

land :: Maybe Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, piece)] ++ after pos board
    where before s = filter isBefore
          after s = filter isAfter
          isBefore s = comp (position s) pos == LT
          isAfter s = comp (position s) pos == GT

comp :: Position -> Position -> Ordering
comp a b =  col a `compare` col b `mappend` (row a `compare` row b)

comp' :: Square -> Square -> Ordering
comp' a b = comp (position a) (position b)

without :: Position -> Board -> Board
without = land Nothing

move :: Position -> Position -> Board -> Board
move from to board = land (fst $ pieceAt from board) to (without from board)

moveWithHistory :: Position -> Position -> [Board] -> [Board]
moveWithHistory from to history = history ++ [newHistory]
    where newHistory = move from to $ last history

pieceAt :: Position -> Board -> (Maybe Piece, Board)
pieceAt pos board = (snd $ head $ filter rightsquare board, board)
    where rightsquare = (==) pos . fst

sort :: Board -> Board
sort = sortBy comp'

emptyBoard :: Board
emptyBoard = Move.sort [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

initBoard = land (Just $ Piece Knight Black)('G',8) $
            land (Just $ Piece Knight Black)('B',8) $
            land (Just $ Piece Bishop Black)('F',8) $
            land (Just $ Piece Bishop Black)('C',8) $
            land (Just $ Piece Rook Black)('H',8) $
            land (Just $ Piece Rook Black)('A',8) $
            land (Just $ Piece Queen Black)('D',8) $
            land (Just $ Piece King Black)('E',8) $
            land (Just $ Piece Pawn Black)('H',7) $
            land (Just $ Piece Pawn Black)('G',7) $
            land (Just $ Piece Pawn Black)('F',7) $
            land (Just $ Piece Pawn Black)('E',7) $
            land (Just $ Piece Pawn Black)('D',7) $
            land (Just $ Piece Pawn Black)('C',7) $
            land (Just $ Piece Pawn Black)('B',7) $
            land (Just $ Piece Pawn Black)('A',7) $
            land (Just $ Piece Knight White)('G',1) $
            land (Just $ Piece Knight White)('B',1) $
            land (Just $ Piece Bishop White)('F',1) $
            land (Just $ Piece Bishop White)('C',1) $
            land (Just $ Piece Rook White)('H',1) $
            land (Just $ Piece Rook White)('A',1) $
            land (Just $ Piece Queen White)('D',1) $
            land (Just $ Piece King White)('E',1) $
            land (Just $ Piece Pawn White)('H',2) $
            land (Just $ Piece Pawn White)('G',2) $
            land (Just $ Piece Pawn White)('F',2) $
            land (Just $ Piece Pawn White)('E',2) $
            land (Just $ Piece Pawn White)('D',2) $
            land (Just $ Piece Pawn White)('C',2) $
            land (Just $ Piece Pawn White)('B',2) $
            land (Just $ Piece Pawn White)('A',2) emptyBoard




