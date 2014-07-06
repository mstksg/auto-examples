{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Process.Random
import Control.Auto.Run
import Control.Auto.Switch
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Prelude hiding              ((.), id)
import System.Random

{-# ANN module "HLint: ignore Use String" #-}
{-# ANN module "HLint: ignore Use string literal" #-}

-- Types
data GCommand = Help
              | Quit
              | HM HMCommand
              deriving Show

data HMCommand = Guess Char
               | Solve String
               | New
               | Display
               deriving Show

data Puzzle = Puzzle { puzzleString :: String   -- The "masked" string
                     , puzzleWrongs :: [Char]   -- List of wrong guesses
                     , puzzleStatus :: Status
                     } deriving Show

data Status = InProgress
            | Success String
            | Failure String
            deriving Show

data PuzzleOut = Puzz Puzzle        -- return current puzzle
               | Swap Puzzle Puzzle -- old puzzle, new puzzle
               deriving Show

-- Config
wordlistFP :: FilePath
wordlistFP = "data/wordlist.txt"

guesses :: Int
guesses = 7

helpmsg :: String
helpmsg = "Solve the hangman!  @new for a new game.  @solve [solution] to "
       <> "solve.  @quit to quit.  @display to show current puzzle.  Type "
       <> "any single character to guess that character."

main :: IO ()
main = do
    putStrLn "Welcome to Hangman!  Type @help for help!"
    wordlist <- lines . map toLower <$> readFile wordlistFP
    g        <- getStdGen

    -- pick an initial word and display it
    let (str0, g') = pick wordlist g
    putStrLn . display $ blankPuzzle str0

    -- here we go
    _  <- interactIdI (hangman wordlist g' str0)

    putStrLn "Goodbye!"

hangman :: Monad m
        => [String]     -- ^ Word list
        -> StdGen       -- ^ Random seed
        -> String       -- ^ Starting word
        -> Auto m String (Maybe String)
hangman wordlist g str0 = proc inp -> do
    -- Primitive command parser
    let comm = case words inp of
                 "@help"   :_      -> Just Help
                 "@quit"   :_      -> Just Quit
                 "@display":_      -> Just (HM Display)
                 "@solve"  :ws     -> Just . HM . Solve
                                    . map toLower . unwords $ ws
                 "@new"    :_      -> Just (HM New)
                 [[c]] | isAlpha c -> Just . HM . Guess . toLower $ c
                 _                 -> Nothing

    case comm of
      Nothing         -> id -< return "Unknown command.  @help for help."
      Just Help       -> id -< return helpmsg
      Just Quit       -> id -< mzero
      Just (HM hcomm) -> do
        -- Make new random strings, in case the puzzle needs it
        newstr <- rands (pick wordlist) g  -< ()

        -- Puzzle, with the command and a fresh string if needed.
        puzz   <- switchF game (game str0) -< (hcomm, newstr)

        -- get wins and losses
        swaps  <- emitOn isSwap              -< puzz
        losses <- countB . filterB isFailure -< swaps
        wins   <- countB . filterB isSuccess -< swaps

        -- display result
        id      -< return $ case puzz of
                              -- just the puzzle
                              Puzz p     -> display p
                              -- the old puzzle and a new puzzle
                              Swap p0 p1 -> display p0
                                         <> "\n"
                                         <> "Wins: " <> show wins <> " | "
                                         <> "Losses: " <> show losses
                                         <> "\n"
                                         <> display p1

game :: Monad m
     => String
     -> Auto m (HMCommand, String) (PuzzleOut, Blip String)
game str = proc (comm, newstr) -> do
    -- get correct guesses, incorrect guesses, and solves
    let (corr, incorr, solve) = case comm of
            Guess c | c `elem` str -> (Just c , Nothing , False)
                    | otherwise    -> (Nothing, Just c  , False)
            Solve s | s == str     -> (Nothing, Nothing , True )
                    | otherwise    -> (Nothing, Just '*', False)
            _                      -> (Nothing, Nothing , False)

    -- collect all correct and wrong guesses
    rights <- mkAccum (++) [' ']         -< maybeToList corr
    wrongs <- reverse <$> mkAccum add [] -< incorr

        -- is it solved?
    let solved = solve || all (`elem` rights) str

        -- did the player run out of guesses?
        failed = length wrongs > guesses

        -- make status
        status | solved    = Success str
               | failed    = Failure str
               | otherwise = InProgress

        -- the puzzle object
        puzz   = Puzzle { puzzleString = map (mask rights) str
                        , puzzleWrongs = wrongs
                        , puzzleStatus = status
                        }

        -- Just p if there should be a new puzzle (from @new or game over)
        mkNew  = case comm of
                   New -> Just (puzz { puzzleStatus = Failure str })
                   _   | solved || failed -> Just puzz
                       | otherwise        -> Nothing

    case mkNew of
      -- new puzzle desired
      Just p' -> do
        let newPuzz = blankPuzzle newstr
        new <- now -< newstr
        id   -< (Swap p' newPuzz, new)

      -- business as usual
      Nothing -> do
        new <- never -< ()
        id   -< (Puzz puzz, new)

  where
    -- add a unique element to a list.  but don't check for uniqueness if
    -- '*' (a bad solve)
    add ws w = case w of
                 Just '*'                -> '*':ws
                 Just c | c `notElem` ws -> c  :ws
                 _                       -> ws

-- pick random element from a list
pick :: [a] -> StdGen -> (a, StdGen)
pick [] _ = error "pick: Cannot pick from empty list."
pick xs g = (xs !! n, g')
  where
    (n, g') = randomR (0, length xs - 1) g

-- new blank puzzle
blankPuzzle :: String -> Puzzle
blankPuzzle str = Puzzle (map (mask []) str) [] InProgress

-- mask item if in the given string, unless it is a space
mask :: String -> Char -> Char
mask _ ' '              = ' '
mask rs c | c `elem` rs = c
          | otherwise   = '_'

-- pretty-print a puzzle
display :: Puzzle -> String
display (Puzzle str ws sts) = pre
                           <> " [" <> str' <> "] "
                           <> "( " <> ws
                           <> replicate (guesses + 1 - length ws) '.'
                           <> ")"
  where
    (pre, str') = case sts of
                    InProgress -> ("Active:", str)
                    Success s  -> ("Solved!", s  )
                    Failure s  -> ("Failed!", s  )

-- because no lens
isSwap :: PuzzleOut -> Bool
isSwap (Swap _ _)  = True
isSwap _           = False

isFailure :: PuzzleOut -> Bool
isFailure (Swap (Puzzle _ _ (Failure _)) _) = True
isFailure _                                 = False

isSuccess :: PuzzleOut -> Bool
isSuccess (Swap (Puzzle _ _ (Success _)) _) = True
isSuccess _                                 = False
