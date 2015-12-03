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
    move ('F',8) ('G',7) $
    move ('A',6) ('B',7) $
    move ('G',7) ('G',6) $
    move ('A',5) ('A',6) $
    move ('G',8) ('F',6) $
    move ('A',4) ('A',5) $
    move ('C',8) ('D',7) $
    move ('A',2) ('A',4) $
    move ('D',7) ('D',5) $
    move ('F',1) ('B',5) $
    move ('E',5) ('D',4) $ -- takes
    move ('D',2) ('D',4) $
    move ('B',8) ('C',6) $
    move ('G',1) ('F',3) $ 
    move ('E',7) ('E',5) $ 
    move ('E',2) ('E',4) initBoard

playRepl :: [Board] -> Color -> IO ()
playRepl boards color = do
    nicePrint (last boards) color
    putStrLn "Enter a move FROM like this : ('E',2)"
    fromPos <- fmap (\s -> read s :: Position) getLine
    putStrLn "Enter a move TO like this : ('E',4)"
    toPos <- fmap (\s -> read s :: Position) getLine
    let legalMoves = moves boards color
    let tryBoard = move fromPos toPos (last boards)
    let newBoard = if tryBoard `elem` legalMoves then tryBoard else last boards
    if tryBoard `elem` legalMoves
        then
            do
                nicePrint newBoard color
                let repliedBoard = replyToMove (invertC color) (boards ++ [newBoard])
                playRepl (replyToMove (invertC color) (boards ++ [newBoard])) color
        else
            playRepl boards color

computerVsComputer :: [Board] -> Color -> IO ()
computerVsComputer boards color = do
    nicePrint (last boards) White
    let newBoard = replyToMove (invertC color) boards
    putStrLn "ENTER to continue"
    _ <- getLine
    computerVsComputer newBoard (invertC color)

main :: IO ()
main = do
    putStrLn "Would you like to play a game of chess?"
    putStrLn "Type White,Black or Computer to play"
    colorString <- getLine
    if colorString /= "White" && colorString /= "Black"
        then
            computerVsComputer [initBoard] White
        else
            do
                let color = read colorString :: Color
                let playBoard = if color == White then [initBoard] else replyToMove White [initBoard]
                playRepl playBoard color

