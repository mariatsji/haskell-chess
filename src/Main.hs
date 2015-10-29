module Main where

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)

import Data.Monoid

import Model
import Printer

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


main :: IO ()
main = do
    print emptyBoard
    print $ land (Pawn White) ('A', 2) emptyBoard
    putStr $ fromString $ prettyPrint (('A',2), Just (Pawn White))
    --putStr $ fromString "♙"


