-- | "Todo"
--
-- A command line client for the todo app in Todo.hs.  At every step, has
-- a primitive instruction parser that parses instructions/commands, sends
-- them into the 'Auto', and outputs the resulting Map of tasks in
-- a pretty-ish way.
--
-- In a GUI, you would have a thread waiting for inputs on a `Chan` queue
-- (using `runOnChan`, for example), and have your GUI elements dump
-- commands into the queue, and render outputs as they come out.

module Main (main) where

import Control.Auto
import Control.Monad
import Data.IntMap           (IntMap)
import Data.Maybe
import Prelude hiding        ((.), id)
import Text.Read
import Todo
import qualified Data.IntMap as IM

-- | Parse a string input.
parseInp :: String -> Maybe TodoInp
parseInp = p . words
  where
    p ("A":xs)   = Just (IAdd (unwords xs))
    p ("D":n:_)  = onId n TEDelete
    p ("C":n:_)  = onId n (TEComplete True)
    p ("U":n:_)  = onId n (TEComplete False)
    p ("P":n:_)  = onId n TEPrune
    p ("M":n:xs) = readMaybe n <&> \i -> ITask i (TEModify (unwords xs))
    p _          = Nothing
    onId :: String -> TaskCmd -> Maybe TodoInp
    onId "*" te = Just (IAll te)
    onId n   te = readMaybe n <&> \i -> ITask i te
    (<&>) :: Functor f => f a -> (a -> b) -> f b
    x <&> f = fmap f x

-- | Just for command line testing use, turning the IntMap into a String.
formatTodo :: IntMap Task -> String
formatTodo = unlines . map format . IM.toList
  where
    format (n, Task desc compl) = concat [ show n
                                         , ". ["
                                         , if compl then "X" else " "
                                         , "] "
                                         , desc
                                         ]

main :: IO ()
main = void . interactAuto $ Just <$> fromBlips ""    -- we need an Interval
                           . perBlip (fmap formatTodo todoApp)
                           . emitJusts parseInp
