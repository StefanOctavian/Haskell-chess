{-# LANGUAGE OverloadedRecordDot, TupleSections #-}
module Bot where

import Moves (Move (..), normalMove)
import GameState (GameState, board, togglePlayingColor, Piece (..), playingColor, lastAdvancedPawn, forwardStepL, secondRowL, lastRowL, isColor, isValidTile)
import Control.Lens (use, (.=), Field2 (_2), Field1 (_1), (%=))
import Data.Maybe (fromJust, isNothing, isJust)
import Helpers (elemAt, (?), Cond ((:?)))
import Control.Monad.State (State, when, modify, MonadState (get), evalState, forM_, unless)
import Data.Functor
import Control.Applicative

recordMove :: Move -> State GameState ()
recordMove move = do
    let (source, dest) = (move.src, move.dst)
    color <- use playingColor
    srcTile <- use (board . elemAt source) <&> fromJust -- the move is valid, so the tile must exist
    destTile <- use $ board . elemAt dest
    let srcPiece = snd srcTile
    let horMove = snd dest - snd source
    let verMove = abs $ fst dest - fst source

    if srcPiece == Pawn then do
        if horMove /= 0 && isNothing destTile then do
            -- en passant
            board . elemAt (fst source, snd source + horMove) .= Nothing
        else when (verMove == 2) $ do
            -- bookkeeping for en passant
            lastAdvancedPawn .= snd source
    else when (srcPiece == King && abs horMove == 2) $ do
        -- castling
        let rookSource = (fst source, horMove > 0 ? 8 :? 1)
        let rookDest = (fst source, horMove > 0 ? 6 :? 4)
        let rookTile = Just (color, Rook)
        board . elemAt rookSource .= Nothing
        board . elemAt rookDest .= rookTile

    let newTile = (color, ) <$> move.replacement <|> destTile
    board . elemAt source .= Nothing
    board . elemAt dest .= newTile
    modify togglePlayingColor

getPieceMoves :: (Int, Int) -> State GameState [Move]
getPieceMoves (row, col) = do
    tile <- use (board . elemAt (row, col)) <&> fromJust
    combinedState <- get <&> ([] :: [Move], )
    let piece = snd tile
    let movesAccumulator = case piece of
            Pawn -> getPawnMoves (row, col)
            Knight -> getKnightMoves (row, col)
            Bishop -> getBishopMoves (row, col)
            Rook -> getRookMoves (row, col)
            Queen -> getQueenMoves (row, col)
            King -> getKingMoves (row, col)
    return $ evalState movesAccumulator combinedState

getPawnMoves :: (Int, Int) -> State ([Move], GameState) [Move]
getPawnMoves source@(row, col) = do
    thisColor <- use (_2 . playingColor)
    secondRow <- use (_2 . secondRowL)
    lastRow <- use (_2 . lastRowL)
    step <- use (_2 . forwardStepL)
    epCol <- use (_2 . lastAdvancedPawn)
    let isEpRow = row == lastRow - 3 * step
    let pushMove = if row == lastRow - step 
        then \dest -> do
            _1 %= (:) (Move source dest (Just Knight))
            _1 %= (:) (Move source dest (Just Queen)) -- no need to promote to rook or bishop
        else \dest -> do _1 %= (:) (normalMove source dest)

    let forward = (row + step, col)
    forwardTile <- (row == lastRow) ? pure Nothing :? use (_2 . board . elemAt forward)
    when (row /= lastRow && isNothing forwardTile) $
        pushMove forward

    let forward2 = (row + 2 * step, col)
    forward2Tile <- (row /= secondRow) ? pure Nothing :? use (_2 . board . elemAt forward2)
    when (row == secondRow && isNothing forwardTile && isNothing forward2Tile) $
        pushMove forward2
        
    let left = (row + step, col - 1)
    let right = (row + step, col + 1)
    leftTile <- (col == 1) ? pure Nothing :? use (_2 . board . elemAt left)
    rightTile <- (col == 8) ? pure Nothing :? use (_2 . board . elemAt right)
    when (col /= 1 && not (isColor leftTile thisColor) || (isEpRow && col - epCol == 1)) $ 
        pushMove left
    when (col /= 8 && not (isColor rightTile thisColor) || (isEpRow && col - epCol == -1)) $ 
        pushMove right
    use _1 -- return the accumulated moves

getKnightMoves :: (Int, Int) -> State ([Move], GameState) [Move]
getKnightMoves (row, col) = do
    thisColor <- use (_2 . playingColor)
    let moves = [(row + i, col + j) | i <- [-2, -1, 1, 2], j <- [-2, -1, 1, 2], 
                isValidTile (row + i, col + j), abs i + abs j == 3]
    forM_ moves $ \dest -> do
        piece <- use (_2 . board . elemAt dest)
        unless (isColor piece thisColor) $ 
            _1 %= (:) (normalMove (row, col) dest)

    use _1

while :: (Monad m) => m Bool -> m ()
while = undefined

getBishopMoves :: (Int, Int) -> State ([Move], GameState) [Move]
getBishopMoves (row, col) = do
    forM_ [(-1, -1), (1, -1), (-1, 1), (1, 1)] $ \(dx, dy) -> do
        let pos = (row + dx, col + dy)
        tile <- isValidTile pos ? 
            use (_2 . board . elemAt (row + dx, col + dy)) :? pure Nothing
        while $ do
            _1 %= (:) (normalMove (row, col) pos) 
            return (isJust tile)
    use _1

getRookMoves :: (Int, Int) -> State ([Move], GameState) [Move]
getRookMoves = undefined

getQueenMoves :: (Int, Int) -> State ([Move], GameState) [Move]
getQueenMoves = undefined

getKingMoves :: (Int, Int) -> State ([Move], GameState) [Move]
getKingMoves = undefined

-- chooses at random a move from the list of moves,
-- records it, and returns it
calculateMove :: State GameState (Maybe Move)
calculateMove = undefined
-- calculateMove = do
--     moves <- concat <$> mapM getPieceMoves [(i, j) | i <- [1..8], j <- [1..8]]
--     if null moves then return Nothing else do
--         let n = length moves
--         i <- random (1, n)
--         let move = moves !! (i - 1)
--         recordMove move
--         return $ Just move