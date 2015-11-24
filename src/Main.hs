module Main where

import Model
import Printer
import Move
import Evaluation
import Board
import Engine
import KingMove

kingMoves =
    land (Just $ Piece King Black)('E',8) $
    land (Just $ Piece King White)('E',1) emptyBoard

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

playRepl :: Board -> Color -> IO ()
playRepl board color = do
    nicePrint board color
    putStrLn "Enter a move FROM like this : ('E',2)"
    fromPos <- fmap (\s -> read s :: Position) getLine
    putStrLn "Enter a move TO like this : ('E',4)"
    toPos <- fmap (\s -> read s :: Position) getLine
    let legalMoves = moves board color
    let tryBoard = move fromPos toPos board
    let newBoard = if tryBoard `elem` legalMoves then tryBoard else board
    if tryBoard `elem` legalMoves
        then
            do
                nicePrint newBoard color
                let repliedBoard = replyToMove (invertC color) newBoard
                playRepl (replyToMove (invertC color) newBoard) color
        else
            playRepl board color

computerVsComputer board color = do
    nicePrint board White
    let newBoard = replyToMove (invertC color) board
    putStrLn "ENTER to continue"
    _ <- getLine
    nicePrint newBoard White
    computerVsComputer newBoard (invertC color)

main :: IO ()
main = do
    putStrLn "Would you like to play a game of chess?"
    putStrLn "Type White,Black or Computer to play"
    colorString <- getLine
    if colorString /= "White" && colorString /= "Black"
        then
            computerVsComputer initBoard White
        else
            do
                let color = read colorString :: Color
                let playBoard = if color == White then initBoard else replyToMove White initBoard
                playRepl playBoard color

testMain = do
    print $ evaluate $ last setup
    putStrLn ""
--    print $ length $ moves kingMoves White
    mapM_ print $ unfilteredMoves (last setup) White
--    mapM_ nicePrint $ moves kingMoves White
--    print $ length $ moves testBoard Black
    print $ "isParalyzed: " ++ show (isParalyzed testBoard Black)
    print $ "isInCheck: " ++ show (isInCheck testBoard Black)
    print $ "isMated: " ++ show (isMated testBoard Black)
    -- mapM_ print $ unfilteredMoves (last setup) White
    -- mapM_ nicePrint $ moves testBoard Black
    -- mapM_ print $ squaresFrom (squareAt ('B',7) $ last setup) (last setup)
    -- print $ pawnSquareFrom ('B',7) (Piece Pawn White) $ last setup
    -- print $ hasOpponentOn White (last setup) ('A',3)
    -- print $ bishopPosFrom ('F',1) (Piece Bishop White) $ last setup

