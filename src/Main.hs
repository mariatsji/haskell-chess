module Main where

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)

import Model
import Printer
import Move
import KnightMove
import Evaluation
import Board


setup =
    moveWithHistory ('E',5) ('D',4) $ -- takes
    moveWithHistory ('D',2) ('D',4) $
    moveWithHistory ('B',8) ('C',6) $
    moveWithHistory ('G',1) ('F',3) $ 
    moveWithHistory ('E',7) ('E',5) $ 
    moveWithHistory ('E',2) ('E',4) [initBoard]

main :: IO ()
main = do
    putStr $ fromString $ prettyBoard $ last setup
    print $ evaluate $ last setup
    print $ isLegal setup
    print $ knightPosFrom ('G',1) (Piece Knight White) initBoard
