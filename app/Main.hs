{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Errors (ChessIOError(..))
import GameState
import Moves (deserializeMove, serializeMove, Move)
import Bot (recordMove, calculateMove)

import Control.Monad (when, forever, unless, forM_)
import Control.Monad.State (StateT, MonadTrans (lift), evalStateT, MonadState (put, get, state), modify, State, runState)
import System.IO (hPutStrLn, stderr, hSetBuffering, stdin, BufferMode (LineBuffering), stdout, hSetEncoding, utf8)
import System.Exit (exitSuccess)
import Control.Lens (view, (.=))
import Control.Exception (throwIO)
import Data.Maybe (isNothing)
import Data.Matrix (prettyMatrix, Matrix)
import Data.Coerce (coerce)


liftState :: (Monad m) => State s a -> StateT s m a
liftState = state . runState

botName :: String
botName = "chessbot"

features :: String
features =
    "feature " ++
    "done=0 " ++
    "sigint=0 " ++
    "sigterm=0 " ++
    "reuse=0 " ++
    "san=0 " ++
    "time=0 " ++
    "usermove=1 " ++
    "analyze=0 " ++
    "ping=0 " ++
    "setboard=0 " ++
    "level=0 " ++
    "variants=\"normal\" " ++
    "name=0 " ++
    "myname=\"" ++ botName ++ "\" " ++
    "done=1\n"

performHandshake :: IO String
performHandshake = do
    xboard <- getLine
    when (xboard /= "xboard") $ throwIO $ HandshakeError "xboard not received"
    putChar '\n'
    protover <- getLine
    when (protover /= "protover 2") $ throwIO $ HandshakeError "protover 2 not received"
    putChar '\n'
    putStrLn features
    let skipToCommand = do
            option <- getLine
            if option `elem` ["new", "force", "go", "quit"] then
                return option
            else
                skipToCommand
    skipToCommand

processMoveStr :: String -> StateT GameState IO ()
processMoveStr moveStr = let move' = deserializeMove moveStr in 
    case move' of 
        Nothing -> performCommand "quit";
        Just move -> processMove move
        
processMove :: Move -> StateT GameState IO ()
processMove move = do
    lift $ hPutStrLn stderr "Good!!!!!!"
    gameState <- get
    liftState $ recordMove move
    lift $ hPutStrLn stderr $ "Board:\n" ++ 
        prettyMatrix (coerce $ view board gameState :: Matrix Tile)
    unless (view forceMode gameState) $ do
        modify togglePlayingColor
        when (view engineColor gameState == NoColor) $
            engineColor .= view playingColor gameState
        nextMove <- liftState calculateMove
        lift $ putStrLn (if isNothing nextMove 
            then "resign "
            else "move " ++ serializeMove nextMove)
    modify togglePlayingColor

performCommand :: String -> StateT GameState IO ()
performCommand "quit" = lift exitSuccess
performCommand "new" = put newGameState
performCommand "force" = do forceMode .= True
performCommand "go" = return () -- TODO: implement
performCommand command = case words command of
    ["usermove", moveStr] -> processMoveStr moveStr
    _                     -> return () -- skip command


startWith :: String -> StateT GameState IO ()
startWith firstCommand = do
    -- new is implicitly performed by setting the initial state
    when (firstCommand /= "new") $
        performCommand firstCommand

    forever $ do
        command <- lift getLine
        lift $ hPutStrLn stderr $ "Received command: " ++ command
        performCommand command

main :: IO ()
main = do
    hSetEncoding stderr utf8
    forM_ [stdin, stdout, stderr] $ \h -> do
        hSetBuffering h LineBuffering

    firstCommand <- performHandshake 
    evalStateT (startWith firstCommand) newGameState