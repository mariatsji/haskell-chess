module Engine (
replyToMove
) where

import Move
import Model
import Evaluation
import Data.List
import Control.Applicative

replyToMove :: Color -> [Board] -> [Board]
replyToMove myColor boards = boards ++ [fst $ 
    head
        $ sortBy (if myColor == White then highestWhiteEval else highestBlackEval)
            (evaluate <$> moves boards myColor)]

highestWhiteEval :: (Board, Float) -> (Board, Float) -> Ordering
highestWhiteEval first second = snd first `compare` snd second

highestBlackEval :: (Board, Float) -> (Board, Float) -> Ordering
highestBlackEval first second = snd second `compare` snd first
