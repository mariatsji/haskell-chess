--Pretty Printer

module Printer (
prettyPrint,
prettyBoard,
nicePrint
) where

import Model
import Data.List
import Prelude
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF

nicePrint :: Board -> Color -> IO ()
nicePrint board color = do
    UP.putStr $ UF.fromString $ prettyBoard board color
    putStrLn " "

prettyPrint :: Square -> String
prettyPrint (p, Nothing)
    | isBlackSquare p =  " "
    | otherwise = " "
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


prettyBoard :: Board -> Color -> String
prettyBoard board color =  intercalate "" $ map prettyRow (toRows board color)

toRows :: Board -> Color -> [Row]
toRows board color = map (toRow board) $ if color == White then reverse [1..8] else [1..8]

toRow :: Board -> Int -> Row
toRow board r = filter (\s -> snd (fst s) == r) board

prettyRow :: Row -> String
prettyRow squares = intercalate "" (fmap prettyPrint squares) ++ "\n"
