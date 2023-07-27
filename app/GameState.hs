{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module GameState where
import Control.Lens hiding (element)
import Data.Matrix (Matrix, matrix)
import Helpers

data Color = White | Black  | NoColor deriving (Eq, Show)
data Piece = Pawn  | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)

newtype Tile = Tile (Maybe (Color, Piece)) deriving (Eq)
instance Show Tile where
    show (Tile (Just (White, Pawn)))   = "♙"
    show (Tile (Just (White, Bishop))) = "♗"
    show (Tile (Just (White, Rook)))   = "♖"
    show (Tile (Just (White, Knight))) = "♘"
    show (Tile (Just (White, Queen)))  = "♕"
    show (Tile (Just (White, King)))   = "♔"
    show (Tile (Just (Black, Pawn)))   = "♟"
    show (Tile (Just (Black, Knight))) = "♞"
    show (Tile (Just (Black, Bishop))) = "♝"
    show (Tile (Just (Black, Rook)))   = "♜"
    show (Tile (Just (Black, Queen)))  = "♛"
    show (Tile (Just (Black, King)))   = "♚"
    show (Tile Nothing) = " "
    show (Tile _) = error "Impossible"

data GameState = GameState {
    _forceMode :: Bool,
    _engineColor :: Color,
    _playingColor :: Color,
    _board :: Matrix (Maybe (Color, Piece)),
    _lastAdvancedPawn :: Int
}

$(makeLenses ''GameState)

newGameState :: GameState
newGameState = GameState {
    _forceMode = False,
    _engineColor = NoColor,
    _playingColor = White,
    _lastAdvancedPawn = 0,
    _board = matrix 8 8 (\(i, j) -> if
        | i == 2    -> Just (White, Pawn)
        | i == 7    -> Just (Black, Pawn)
        | i == 1    -> Just (White, pieceAt j)
        | i == 8    -> Just (Black, pieceAt j)
        | otherwise -> Nothing)
} where 
    pieceAt j
        | j == 1 || j == 8 = Rook
        | j == 2 || j == 7 = Knight
        | j == 3 || j == 6 = Bishop
        | j == 4           = Queen
        | j == 5           = King
        | otherwise        = error "Impossible"

togglePlayingColor :: GameState -> GameState
togglePlayingColor = over playingColor toggle where
    toggle White = Black
    toggle Black = White
    toggle NoColor = NoColor

firstRowL :: Getting Int GameState Int
firstRowL = playingColor . to (\color -> color == White ? 1 :? 8)

secondRowL :: Getting Int GameState Int
secondRowL = playingColor . to (\color -> color == White ? 2 :? 7)

lastRowL :: Getting Int GameState Int
lastRowL = playingColor . to (\color -> color == White ? 8 :? 1)

forwardStepL :: Getting Int GameState Int
forwardStepL = playingColor . to (\color -> color == White ? 1 :? -1)

isColor :: Maybe (Color, Piece) -> Color -> Bool
isColor (Just (color, _)) color' = color == color'
isColor _ _ = False

isValidTile :: (Int, Int) -> Bool
isValidTile (row, col) = row >= 1 && row <= 8 && col >= 1 && col <= 8