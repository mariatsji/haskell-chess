module Main where

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)

import Data.Monoid

data Color = White | Black deriving (Eq,Show,Ord,Read,Enum,Bounded)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq,Show,Ord,Read)
type Position = (Char, Int)
type Square = (Position, Maybe Piece)
type Board = [Square]

emptyBoard :: Board
emptyBoard = [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

land :: Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, Just piece)] ++ after pos board
    where before pos board = filter isBefore board
          after pos board = filter isAfter board
          comp a b =  (col a) `compare` (col b) `mappend` ((row a) `compare` (row b))
          isBefore s = comp (position s) pos == LT
          isAfter s = comp (position s) pos == GT

row :: Position -> Int
row pos = snd pos

col :: Position -> Char
col pos = fst pos

position :: Square -> Position
position s = fst s

prettyPrint :: Square -> String
prettyPrint (_, Nothing) = " "
prettyPrint (_, Just (Pawn White)) = "♙"
prettyPrint (_, Just (Knight White)) = "♘"
prettyPrint (_, Just (Bishop White)) = "♗"
prettyPrint (_, Just (Rook White)) = "♖"
prettyPrint (_, Just (Queen White)) = "♕"
prettyPrint (_, Just (King White)) = "♔"
prettyPrint (_, Just (Pawn Black)) = "♟"
prettyPrint (_, Just (Knight Black)) = "♞"
prettyPrint (_, Just (Bishop Black)) = "♝"
prettyPrint (_, Just (Rook Black)) = "♜"
prettyPrint (_, Just (Queen Black)) = "♛"
prettyPrint (_, Just (King Black)) = "♚"


main :: IO ()
main = do
    print emptyBoard
    print $ land (Pawn White) ('A', 2) emptyBoard
    putStr $ fromString $ prettyPrint (('A',2), Just (Pawn White))
    --putStr $ fromString "♙"


