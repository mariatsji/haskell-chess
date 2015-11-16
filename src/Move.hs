-- Moves and stuff

module Move (
land,
row,
col,
position,
pieceAt,
move,
moves,
moveWithHistory,
positionsFrom,
isLegal
) where

import Model
import Data.Monoid
import Data.List
import Data.Char
import Data.Maybe
import KnightMove
import PawnMove

moves :: Board -> Color -> [Board]
moves board color = [move (position square) toPos board |
    square <- filter (hasColoredP color) board,
    toPos <- positionsFrom square board,
    isLegal board]

positionsFrom :: Square -> Board -> [Position]
positionsFrom (p,mp) b = case mp of Nothing -> []
                                    Just piece -> positionsFrom' p piece b

positionsFrom' :: Position -> Piece -> Board -> [Position]
positionsFrom' position piece board =
    filter insideBoard $ case piece of Piece Knight _ -> knightPosFrom position piece board
                                       Piece Pawn _ -> pawnPosFrom position piece board
                                       otherwise -> []

isLegal :: Board -> Bool
isLegal = oneKingEach

oneKingEach :: Board -> Bool
oneKingEach b = not ( null $ findPiece (Piece King White) b) &&
    not (null $ findPiece (Piece King Black) b)

findPiece :: Piece -> Board -> [Square]
findPiece piece = filter (`hasPiece` piece)

hasPiece :: Square -> Piece -> Bool
hasPiece s p = snd s == Just p

land :: Maybe Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, piece)] ++ after pos board
    where before x = filter isBefore
          after y = filter isAfter
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

occupied :: Position -> Piece -> Board -> Bool
occupied pos pi board = Just (pColor pi) == fmap pColor ( fst $ pieceAt pos board)
