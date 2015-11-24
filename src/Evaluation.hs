module Evaluation (
evaluate
) where

import Model

evaluate :: Board -> (Board, Float)
evaluate board = (board, weight board)

weight :: Board -> Float
weight board = material board + pawnAdvanceBonus board

material :: Board -> Float
material board = sum $ map toValue board

toValue :: Square -> Float
toValue s = case s of (_, Nothing) -> 0.0
                      (_, Just (Piece Pawn White)) -> 1.0
                      (_, Just (Piece Knight White)) -> 3.0
                      (_, Just (Piece Bishop White)) -> 3.0
                      (_, Just (Piece Rook White)) -> 5.0
                      (_, Just (Piece Queen White)) -> 9.0
                      (_, Just (Piece King White)) -> 50.0
                      (_, Just (Piece Pawn Black)) -> -1.0
                      (_, Just (Piece Knight Black)) -> -3.0
                      (_, Just (Piece Bishop Black)) -> -3.0
                      (_, Just (Piece Rook Black)) -> -5.0
                      (_, Just (Piece Queen Black)) -> -9.0
                      (_, Just (Piece King Black)) -> -50.0

pawnAdvanceBonus :: Board -> Float
pawnAdvanceBonus board = sum $ map toPawnRowBonus board

toPawnRowBonus :: Square -> Float
toPawnRowBonus s = case s of (_, Just (Piece Pawn White)) -> realToFrac (row (position s) - 1) * 0.1
                             (_, Just (Piece Pawn Black)) -> realToFrac (7 - row (position s)) * (-0.1)
                             otherwise -> 0.0
