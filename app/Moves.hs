{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Moves where

import Data.Char (ord, chr)
import Control.Exception (throw)
import Errors (ChessIOError(..))
import GameState (Piece (Queen, Rook, Bishop, Knight))

data Move = Move {
    src :: (Int, Int),
    dst :: (Int, Int),
    replacement :: Maybe Piece
} deriving (Eq, Show)

isNormalMove :: Move -> Bool
isNormalMove (Move { replacement = Nothing }) = True
isNormalMove _ = False

isPromotion :: Move -> Bool
isPromotion = not . isNormalMove

normalMove :: (Int, Int) -> (Int, Int) -> Move
normalMove x y = Move x y Nothing

deserializeMove :: String -> Maybe Move
deserializeMove "resign" = Nothing
deserializeMove (col1:row1:col2:row2:prom) =
    let source = (ord row1 - ord 'a', ord col1 - ord '0')
        dest   = (ord row2 - ord 'a', ord col2 - ord '0') in
    Just $ Move source dest (if null prom then Nothing else Just $ letterToPiece $ head prom)
deserializeMove str = throw $ InvalidMoveFormatError $ "Found " ++ str

serializeMove :: Maybe Move -> String
serializeMove Nothing = "resign"
serializeMove (Just move) =
    let (row1, col1) = move.src
        (row2, col2) = move.dst in
    [chr (col1 + ord '0'), chr (row1 + ord 'a'),
     chr (col2 + ord '0'), chr (row2 + ord 'a'),
     maybe ' ' pieceToLetter move.replacement]

letterToPiece :: Char -> Piece
letterToPiece 'q' = Queen
letterToPiece 'r' = Rook
letterToPiece 'b' = Bishop
letterToPiece 'n' = Knight
letterToPiece _   = error "Invalid piece"

pieceToLetter :: Piece -> Char
pieceToLetter Queen  = 'q'
pieceToLetter Rook   = 'r'
pieceToLetter Bishop = 'b'
pieceToLetter Knight = 'n'
pieceToLetter _      = error "Invalid piece"

