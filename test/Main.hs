module Main where

import Lib

data Color = White | Black deriving (Eq,Show,Ord,Read,Enum,Bounded)

data Piece = Pawn Color | Knight Color deriving (Eq,Show,Ord,Read)

type Position = (Char, Int)

type Square = (Position, Maybe Piece)

type Board = [Square]

emptyBoard :: Board
emptyBoard = [((col, row), Nothing) | col <- ['A'..'H'], row <- [1..8]]

land :: Piece -> Position -> Board -> Board
land piece pos board = before pos board ++ [(pos, Just piece)] ++ after pos board

row :: Position -> Int
row pos = snd pos

col :: Position -> Char
col pos = fst pos

position :: Square -> Position
position s = fst s

comp:: Position -> Position -> Ordering
comp a b =  (col a) `compare` (col b) `mappend` ((row a) `compare` (row b))

before :: Position -> Board -> [Square]
before pos board = filter (\s -> comp (position s) pos == LT) board

after :: Position -> Board -> [Square]
after pos board = filter (\s -> comp (position s) pos == GT) board

--prettyPrint :: Square -> Char
--prettyPrint (pos, piece) = case piece of Just Pawn White -> \u2659
--                                         Just Knight White -> \u2658
--                                         Nothing -> ' '


main :: IO ()
main = do
    print emptyBoard
    print $ land (Pawn White) ('A', 2) emptyBoard
    putStrLn "\u2659"


