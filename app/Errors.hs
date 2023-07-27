module Errors where
import Data.Data (Typeable)
import Control.Exception (Exception)

data ChessIOError =
    HandshakeError String |
    InvalidMoveFormatError String
    deriving (Eq, Typeable)

instance Show ChessIOError where
    show (HandshakeError s) = "HandshakeError: " ++ s
    show (InvalidMoveFormatError s) = "InvalidMoveFormatError: " ++ s

instance Exception ChessIOError