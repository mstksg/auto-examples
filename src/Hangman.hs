{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Process.Random
import Control.Auto.Run
import Data.Functor.Identity
import Control.Auto.Serialize
import Control.Exception hiding (mask)
import Data.Foldable (mapM_)
import Control.Auto.Switch
import Control.Monad hiding (mapM_)
import Data.Char
import Data.List
import Data.Maybe
import Prelude hiding              ((.), id, interact, mapM_)
import System.Random

{-# ANN Puzzle "HLint: ignore Use String" #-}
{-# ANN game "HLint: ignore Use string literal" #-}

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

data PuzzleOut = Puzz Puzzle Bool   -- return current puzzle; show score?
               | Swap Puzzle Puzzle -- old puzzle, new puzzle
               deriving Show

-- Config
wordlistFP :: FilePath
wordlistFP = "data/wordlist.txt"

savegameFP :: FilePath
savegameFP = "data/save/hangman"

guesses :: Int
guesses = 7

helpmsg :: String
helpmsg = unlines [ "Solve the hangman!"
                  , "> @new                   : new game                "
                  , "> @help                  : display this message    "
                  , "> @display               : display score and puzzle"
                  , "> (any single character) : guess that character    "
                  , "> @solve [sol]           : attempt to solve        "
                  , "> @quit                  : quit                    "
                  ]


main :: IO ()
main = do
    putStrLn "Welcome to Hangman!  Type @help for help!"
    wordlist <- lines . map toLower <$> readFile wordlistFP
    g        <- getStdGen

    -- Our game Auto; `hangman` with a wordlist and a starting seed
    let gameAuto = hangman wordlist g :: Auto Identity String (Maybe String)

    -- Attempt to load the savefile
    loaded <- try (readAuto savegameFP gameAuto)

    -- loadedGame is the loaded/deserialized game auto
    loadedGame <- case loaded of
      Right (Right a)           -> do
        putStrLn "Save file found!  Restoring game."
        return a
      Left (_ :: SomeException) -> do
        putStrLn "No save file found; creating new game."
        return gameAuto
      _                         -> do
        putStrLn "Save file corrupted; creating new game."
        return gameAuto

    -- run through one iteration to output the current puzzle
    --   `initGame` is the game auto after going through one step
    let Output str initGame = runIdentity (stepAuto loadedGame "@display")

    -- print out out the current puzzle
    mapM_ putStrLn str

    -- here we go, start running the loop with the initialized game auto
    --   `finalGame` is the game after the loop has ended.
    finalGame  <- interactId initGame

    -- save the game; serialize and write `finalGame`.
    putStrLn $ "Saving game to " <> savegameFP <> "..."
    writeAuto savegameFP finalGame

    putStrLn "Goodbye!"

-- the main game auto
hangman :: Monad m
        => [String]     -- ^ Word list
        -> StdGen       -- ^ Random seed
        -> Auto m String (Maybe String)
        --        ^       ^
        --        |       +-- Command line output.  Nothing means quit.
        --        +-- Command line input
hangman wordlist g = proc inp -> do
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
      -- return is Just; mzero is Nothing
      Nothing         -> id -< return "Unknown command.  @help for help."
      Just Help       -> id -< return helpmsg
      Just Quit       -> id -< mzero
      Just (HM hcomm) -> do
        -- Make new random strings, in case the puzzle needs it
        newstr <- stdRands (pick wordlist) g -< ()

        -- Puzzle, with the command and a fresh string if needed.
        --   `switchFromF` basically creates a new "game" with a new word
        --   every time the internal wire emits a blip containing a new
        --   word.
        puzz   <- switchFromF game initialize -< (hcomm, newstr)

        -- get wins and losses
        swaps  <- emitOn isSwap              -< puzz
        losses <- countB . filterB isFailure -< swaps
        wins   <- countB . filterB isSuccess -< swaps

        -- display result
        id      -< return $ case puzz of
                              -- just the puzzle
                              Puzz p False -> display p
                              -- puzzle + score
                              Puzz p True  -> displayScore (wins, losses)
                                           <> "\n"
                                           <> display p
                              -- the old puzzle and a new puzzle
                              Swap p0 p1   -> display p0
                                           <> "\n"
                                           <> displayScore (wins, losses)
                                           <> "\n"
                                           <> display p1

-- initial game...only here to "start" the real `game` auto.  All it does
--   is emit a `blip` with a word, which causes `switchFromF` to create
--   a new game with that word.
initialize :: Monad m
           => Auto m (HMCommand, String) (PuzzleOut, Blip String)
initialize = proc (_, newstr) -> do
    new <- immediately -< newstr
    id   -< (Puzz (blankPuzzle newstr) True, new)

-- A single game with a single word.  Takes in commands and outputs
--   `PuzzleOut`s...or a blip containing the next mystery word.  `switchF`
--   takes this blip and creates a fresh `game` out of it, starting the
--   cycle all over.
game :: Monad m
     => String    -- ^ The mystery word(s)
     -> Auto m (HMCommand, String) (PuzzleOut, Blip String)
     --         ^          ^        ^          ^
     --         |          |        |          +-- Event signaling new game, with new word
     --         |          |        +-- Output puzzle (or puzzle swap)
     --         |          +-- New random word, if needed to make a new game
     --         +-- Hangman command
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
        new <- immediately -< newstr
        id   -< (Swap p' newPuzz, new)

      -- business as usual
      Nothing -> do
        -- show the score if @display was the command
        let showScore = isDisplay comm

        new <- never -< ()
        id   -< (Puzz puzz showScore, new)

  where
    -- add a unique element to a list.  but don't check for uniqueness if
    --   '*' (a bad solve)
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

-- Pretty print a puzzle
display :: Puzzle -> String
display (Puzzle str ws sts) = pre
                           <> " [" <> str' <> "] "
                           <> "(" <> ws
                           <> replicate (guesses + 1 - length ws) '.'
                           <> ")"
  where
    (pre, str') = case sts of
                    InProgress -> ("Active:", str)
                    Success s  -> ("Solved!", s  )
                    Failure s  -> ("Failed!", s  )

-- Pretty print the score
displayScore :: (Int, Int) -> String
displayScore (w, l) = unwords ["Wins:", show w, "|", "Losses:", show l]

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

isDisplay :: HMCommand -> Bool
isDisplay Display = True
isDisplay _       = False
