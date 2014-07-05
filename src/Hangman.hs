{-# LANGUAGE Arrows #-}

module Main (main) where

-- import Control.Arrow
-- import Data.Functor
import Control.Auto
import Control.Auto.Event.Internal
import Control.Auto.Generate
import Data.Maybe
import Data.Char
import Control.Auto.Event
import Control.Auto.Effects
import Control.Auto.Run
import System.Random
import Control.Auto.Switch
import Control.Monad
import Prelude hiding       ((.), id)

data GCommand = Help
              | Quit
              | HM HMCommand
              deriving Show

data HMCommand = Guess Char
               | Solve String
               | New   String
               deriving Show

data Puzzle = Puzzle { puzzleString :: String
                     , puzzleMisses :: String
                     , puzzleSatus  :: Status
                     } deriving Show

data Status = InProgress
            | Success String
            | Failure String
            deriving Show

wordlistFP :: FilePath
wordlistFP = "data/wordlist.txt"

helpmsg :: String
helpmsg = "Solve the hangman!"

main :: IO ()
main = do
    putStrLn "Welcome to Hangman!  Type @help for help!"
    wordlist <- lines . map toLower <$> readFile wordlistFP
    g        <- getStdGen
    _        <- interactIdI (hangman wordlist g)
    putStrLn "Goodbye!"

hangman :: Monad m
        => [String]
        -> StdGen
        -> Auto m String (Maybe String)
hangman wordlist g = proc inp -> do

    comm    <- case words inp of
                 "@quit" :_        -> id -< Quit
                 "@solve":ws       -> id -< HM $ Solve (unwords ws)
                 "@new"  :_        -> do
                     w <- mkState_ (\_ -> pick wordlist) g -< ()
                     id -< HM $ New w
                 [[c]] | isAlpha c -> id -< HM $ Guess (toLower c)
                 _                 -> id -< Help

    case comm of
      Help     -> id -< return helpmsg
      Quit     -> id -< mzero
      HM hcomm -> do
        puzz <- switchF game (game Nothing) -< hcomm
        id    -< Just (show puzz)


game :: Monad m => Maybe String -> Auto m HMCommand (Maybe Puzzle, Event (Maybe String))
game mstr0 = proc comm ->
    case (mstr0, comm) of
      (_, New str1) -> do
        let newPuzz = Puzzle ('_' <$ str1) "" InProgress
        new <- now -< Just str1
        id -< (Just newPuzz, new)
      (Nothing, _)  -> do
        e <- never -< ()
        id -< (Nothing, e)
      (Just str0, _)     -> do
        id -< (Just $ Puzzle str0 "" InProgress, NoEvent)


pick :: [a] -> StdGen -> (a, StdGen)
pick [] _ = error "pick: Cannot pick from empty list."
pick xs g = (xs !! n, g')
  where
    (n, g') = randomR (0, length xs - 1) g
