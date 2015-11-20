module Main where

import Model
import Printer
import Move
--import KnightMove
--import BishopMove
import Evaluation
import Board
import Engine


setup =
    moveWithHistory ('E',5) ('D',4) $ -- takes
    moveWithHistory ('D',2) ('D',4) $
    moveWithHistory ('B',8) ('C',6) $
    moveWithHistory ('G',1) ('F',3) $ 
    moveWithHistory ('E',7) ('E',5) $ 
    moveWithHistory ('E',2) ('E',4) [initBoard]

repl :: Board -> IO ()
repl board = do
    nicePrint board
    putStrLn "Enter a move FROM like this : ('E',2)"
    fromString <- getLine
    putStrLn "Enter a move TO like this : ('E',4)"
    toString <- getLine
    let fromPos = read fromString :: Position
    let toPos = read toString :: Position
    let newBoard = move fromPos toPos board
    nicePrint newBoard
    let repliedBoard = replyToMove Black newBoard
    repl repliedBoard

main :: IO ()
main = do
    putStrLn "Would you like to play a game of chess?"
    repl initBoard

--main = do
 --    nicePrint $ last setup
    --putStr $ fromString $ prettyBoard $ last setup
    --print $ evaluate $ last setup
    --print $ isLegal $ head setup
    -- print $ knightPosFrom ('G',1) (Piece Knight White) initBoard
  --  putStrLn ""
   -- mapM_ nicePrint $ moves (last setup) White
   -- print $ length $ moves (last setup) White
    -- print $ hasOpponentOn White (last setup) ('A',3)
    -- print $ bishopPosFrom ('F',1) (Piece Bishop White) $ last setup

