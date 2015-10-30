module Main where

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)

import Model
import Printer
import Engine

main :: IO ()
main = putStr $ fromString $ prettyBoard initBoard
    --putStr $ fromString $ prettyPrint (('A',2), Just (Pawn White))
    --putStr $ fromString "♙"


