module RandomEngine (
replyToMove
) where

import Move
import Model
import Evaluation
import System.Random


replyToMove :: Color -> [Board] -> [Board]
replyToMove myColor boards = boards ++ [randomFrom $  moves boards myColor]

randomFrom :: [Board] -> Board
randomFrom boards = boards !! randomIndex boards

randomIndex :: [Board] -> Int
randomIndex boards = fst (randomR (0, length boards - 1) (mkStdGen 100))
