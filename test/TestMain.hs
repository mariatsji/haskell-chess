module Main where

import Chess
import Printer
import Move
import Evaluation
import Board
import Engine
import KnightMove
import BishopMove


setup =
    moveWithHistory ('E',5) ('D',4) $ -- takes
    moveWithHistory ('D',2) ('D',4) $
    moveWithHistory ('B',8) ('C',6) $
    moveWithHistory ('G',1) ('F',3) $ 
    moveWithHistory ('E',7) ('E',5) $ 
    moveWithHistory ('E',2) ('E',4) [initBoard]

main = do
    nicePrint $ last setup
    putStr $ fromString $ prettyBoard $ last setup
    print $ evaluate $ last setup
    print $ isLegal $ head setup
    print $ knightPosFrom ('G',1) (Piece Knight White) initBoard
    putStrLn ""
    mapM_ nicePrint $ moves (last setup) White
    print $ length $ moves (last setup) White
    print $ hasOpponentOn White (last setup) ('A',3)
    print $ bishopPosFrom ('F',1) (Piece Bishop White) $ last setup

