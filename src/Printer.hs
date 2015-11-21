--Pretty Printer

module Printer (
prettyPrint,
prettyBoard,
nicePrint
) where

import Model
import Data.List
import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)

nicePrint :: Board -> IO ()
nicePrint board = do
    putStr $ fromString $ prettyBoard board
    putStrLn " "

prettyPrint :: Square -> String
prettyPrint (p, Nothing)
    | isBlackSquare p =  "   "
    | otherwise = "   "
prettyPrint (_, Just (Piece Pawn White)) = "♙"
prettyPrint (_, Just (Piece Knight White)) = "♘"
prettyPrint (_, Just (Piece Bishop White)) = "♗"
prettyPrint (_, Just (Piece Rook White)) = "♖"
prettyPrint (_, Just (Piece Queen White)) = "♕"
prettyPrint (_, Just (Piece King White)) = "♔"
prettyPrint (_, Just (Piece Pawn Black)) = "♟"
prettyPrint (_, Just (Piece Knight Black)) = "♞"
prettyPrint (_, Just (Piece Bishop Black)) = "♝"
prettyPrint (_, Just (Piece Rook Black)) = "♜"
prettyPrint (_, Just (Piece Queen Black)) = "♛"
prettyPrint (_, Just (Piece King Black)) = "♚"


prettyBoard :: Board -> String
prettyBoard board =  intercalate "" $ map prettyRow (toRows board)

toRows :: Board -> [Row]
toRows board = map (toRow board) $ reverse [1..8]

toRow :: Board -> Int -> Row
toRow board r = filter (\s -> snd (fst s) == r) board

prettyRow :: Row -> String
prettyRow squares = intercalate "" (fmap prettyPrint squares) ++ "\n"
