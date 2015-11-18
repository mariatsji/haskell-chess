module Engine (
replyToMove
) where

import Move
import Model
import Evaluation
import Data.List
import Control.Applicative

replyToMove :: Color -> Board -> Board
replyToMove myColor board = fst $ head $ sortBy highestEval (evaluate <$> moves board myColor)

highestEval :: (Board, Float) -> (Board, Float) -> Ordering
highestEval first second = snd first `compare` snd second
