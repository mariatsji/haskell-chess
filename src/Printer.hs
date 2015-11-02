--Pretty Printer

module Printer (
prettyPrint,
prettyBoard
) where

import Model
import Data.List

prettyPrint :: Square -> String
prettyPrint (_, Nothing) = "   "
prettyPrint (_, Just (Pawn White)) = "♙"
prettyPrint (_, Just (Knight White)) = "♘"
prettyPrint (_, Just (Bishop White)) = "♗"
prettyPrint (_, Just (Rook White)) = "♖"
prettyPrint (_, Just (Queen White)) = "♕"
prettyPrint (_, Just (King White)) = "♔"
prettyPrint (_, Just (Pawn Black)) = "♟"
prettyPrint (_, Just (Knight Black)) = "♞"
prettyPrint (_, Just (Bishop Black)) = "♝"
prettyPrint (_, Just (Rook Black)) = "♜"
prettyPrint (_, Just (Queen Black)) = "♛"
prettyPrint (_, Just (King Black)) = "♚"

prettyBoard :: Board -> String
prettyBoard board =  intercalate "" $ map prettyRow (toRows board)

toRows :: Board -> [Row]
toRows board = map (toRow board) $ reverse [1..8]

toRow :: Board -> Int -> Row
toRow board r = filter (\s -> snd (fst s) == r) board

prettyRow :: Row -> String
prettyRow squares = intercalate "" (fmap prettyPrint squares) ++ "\n"
