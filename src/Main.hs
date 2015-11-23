module Main where

import Model
import Printer
import Move
import Evaluation
import Board
import Engine
import KingMove


setup =
    moveWithHistory ('F',8) ('G',7) $
    moveWithHistory ('A',6) ('B',7) $
    moveWithHistory ('G',7) ('G',6) $
    moveWithHistory ('A',5) ('A',6) $
    moveWithHistory ('G',8) ('F',6) $
    moveWithHistory ('A',4) ('A',5) $
    moveWithHistory ('C',8) ('D',7) $
    moveWithHistory ('A',2) ('A',4) $
    moveWithHistory ('D',7) ('D',5) $
    moveWithHistory ('F',1) ('B',5) $
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

--main :: IO ()
--main = do
--    putStrLn "Would you like to play a game of chess?"
--    repl initBoard

main = do
    nicePrint testBoard
    --putStr $ fromString $ prettyBoard $ last setup
    --print $ evaluate $ last setup
    --print $ isLegal $ head setup
    -- print $ knightPosFrom ('G',1) (Piece Knight White) initBoard
  --  putStrLn ""
    print $ length $ moves testBoard Black
    print $ "isParalyzed: " ++ show (isParalyzed testBoard Black)
    print $ "isInCheck: " ++ show (isInCheck testBoard Black)
    print $ "isMated: " ++ show (isMated testBoard Black)
    -- mapM_ print $ unfilteredMoves (last setup) White
    -- mapM_ nicePrint $ moves testBoard Black
    -- mapM_ print $ squaresFrom (squareAt ('B',7) $ last setup) (last setup)
    -- print $ pawnSquareFrom ('B',7) (Piece Pawn White) $ last setup
    -- print $ hasOpponentOn White (last setup) ('A',3)
    -- print $ bishopPosFrom ('F',1) (Piece Bishop White) $ last setup

