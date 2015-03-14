
module Main (main) where

import Control.Auto
import Control.Monad
import Data.Map           (Map)
import Data.Maybe
import Prelude hiding     ((.), id)
import Text.Read
import Todo
import qualified Data.Map as M

-- | Parse a string input.  Just for testing.  Ideally, these events will
-- come from a GUI.
parseInp :: String -> Maybe InpEvent
parseInp = p . words
  where
    p ("A":xs)   = Just (IEAdd (unwords xs))
    p ("D":n:_)  = onId n TEDelete
    p ("C":n:_)  = onId n (TEComplete True)
    p ("U":n:_)  = onId n (TEComplete False)
    p ("P":n:_)  = onId n TEPrune
    p ("M":n:xs) = readMaybe n <&> \i -> IETask i (TEModify (unwords xs))
    p _          = Nothing
    onId :: String -> TaskCmd -> Maybe InpEvent
    onId "*" te = Just (IEAll te)
    onId n   te = readMaybe n <&> \i -> IETask i te
    (<&>) :: Functor f => f a -> (a -> b) -> f b
    x <&> f = fmap f x

-- | Just for command line testing use, turning the Map into a String.
-- Ideally this would be handled by a GUI.
formatTodo :: Map TaskID Task -> String
formatTodo = unlines . map format . M.toList
  where
    format (n, Task desc compl) = concat [ show n
                                         , ". ["
                                         , if compl then "X" else " "
                                         , "] "
                                         , fromMaybe "" desc
                                         ]

main :: IO ()
main = void . interactAuto $ Just <$> fromBlips ""
                           . perBlip (fmap formatTodo todoApp)
                           . emitJusts parseInp
