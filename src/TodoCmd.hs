
module Main (main) where

import Control.Auto
import Control.Auto.Blip
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
    p ("D":n:_)  = readMaybe n <&> \n' -> IETask n' TEDelete
    p ("C":n:_)  = readMaybe n <&> \n' -> IETask n' (TEComplete True)
    p ("U":n:_)  = readMaybe n <&> \n' -> IETask n' (TEComplete False)
    p ("M":n:xs) = readMaybe n <&> \n' -> IETask n' (TEModify (unwords xs))
    p _          = Nothing
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
                           . perBlip (fmap formatTodo taskInp)
                           . emitJusts parseInp
