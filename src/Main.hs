module Main where

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)

import Model
import Printer
import Engine

main :: IO ()
main = putStr $ fromString $ prettyBoard $
    move ('B',8) ('C',6) $
    move ('G',1) ('F',3) $
    move ('E',7) ('E',5) $
    move ('E',2) ('E',4) initBoard
    --putStr $ fromString $ prettyPrint (('A',2), Just (Pawn White))
    --putStr $ fromString "♙"


