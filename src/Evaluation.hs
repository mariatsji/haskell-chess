module Evaluation (
evaluate
) where

import Model

evaluate :: Board -> (Board, Float)
evaluate board = (board, weight board)

weight :: Board -> Float
weight board = sum $ map toValue board

toValue :: Square -> Float
toValue s = case s of (_, Nothing) -> 0.0
                      (_, Just (Pawn White)) -> 1.0
                      (_, Just (Knight White)) -> 3.0
                      (_, Just (Bishop White)) -> 3.0
                      (_, Just (Rook White)) -> 5.0
                      (_, Just (Queen White)) -> 9.0
                      (_, Just (King White)) -> 50.0
                      (_, Just (Pawn Black)) -> -1.0
                      (_, Just (Knight Black)) -> -3.0
                      (_, Just (Bishop Black)) -> -3.0
                      (_, Just (Rook Black)) -> -5.0
                      (_, Just (Queen Black)) -> -9.0
                      (_, Just (King Black)) -> -50.0


