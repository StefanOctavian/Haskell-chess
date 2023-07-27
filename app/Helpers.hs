{-# LANGUAGE RankNTypes #-}
module Helpers where
import Control.Lens (Lens', lens)
import Data.Matrix (Matrix, setElem, getElem)
import Control.Monad.State (State, MonadState (get), when)
import Data.Maybe (isJust, fromJust)

elemAt :: (Int, Int) -> Lens' (Matrix a) a
elemAt (i, j) = lens (getElem i j) (\m a -> setElem a (i, j) m)

data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

toMaybe :: a -> Bool -> Maybe a
toMaybe x True = Just x
toMaybe _ False = Nothing

(~+~) :: (Num t1, Num t2) => (t1, t2) -> (t1, t2) -> (t1, t2)
(x1, y1) ~+~ (x2, y2) = (x1 + x2, y1 + y2)

maybeApplyA :: (Applicative m) => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
maybeApplyA Nothing _ = pure Nothing
maybeApplyA (Just x) f = f x

whileS :: (s -> Bool) -> State s () -> State s ()
whileS cond action = do
    st <- get
    let condResult = cond st
    when condResult $  do
        action
        whileS cond action

while :: (Monad m) => m Bool -> m ()
while action = action >>= flip when (while action)

whileJust :: (Monad m) => m (Maybe a) -> (a -> m b) -> m [b]
whileJust condition body = do
    let 
     loop result = do
        maybeItem <- condition
        case maybeItem of
            Just item -> do
                stepRes <- body item
                loop (stepRes : result)
            Nothing -> return result
    reverse <$> loop []

whileJust_ :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whileJust_ condition body = do
    maybeItem <- condition
    when (isJust maybeItem) $ do
        body $ fromJust maybeItem
        whileJust_ condition body

