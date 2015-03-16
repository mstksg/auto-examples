{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module Main (main) where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Effects
import Control.Auto.Interval
import Control.Auto.Switch
import Control.Exception hiding  (mask)
import Control.Monad hiding      (mapM_, sequence)
import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Char
import Data.Foldable             (mapM_)
import Data.List
import Data.Maybe
import Data.Serialize
import Data.Traversable          (sequence)
import GHC.Generics
import Prelude hiding            ((.), id, mapM_, sequence)

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
                     } deriving (Show, Generic)

data Status = InProgress
            | Success String
            | Failure String
            deriving (Show, Generic)

data PuzzleOut = Puzz Puzzle Bool   -- return current puzzle; show score?
               | Swap Puzzle Puzzle -- old puzzle, new puzzle
               deriving (Show, Generic)

instance Serialize Puzzle
instance Serialize Status
instance Serialize PuzzleOut

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
    let gameAuto = hangman wordlist g :: Auto' String (Maybe String)

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
    let (str, initGame) = stepAuto' loadedGame "@display"

    -- print out out the current puzzle
    mapM_ putStrLn str

    -- here we go, start running the loop with the initialized game auto
    --   `finalGame` is the game after the loop has ended.
    finalGame  <- interactAuto initGame

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
-- hangmanRandom runs under `RandT`/a `MonadRandom`, so we "seal away" the
-- randomness with `sealRandom`; see "Control.Auto.Random" documentation
-- for more information, and also `sealState`.  Lets the random parts run
-- by themselves and become inaccessible to the outside world.
hangman wordlist g0 = sealRandom_ hangmanRandom g1
  where
    firstGoal :: String
    g1 :: StdGen
    (firstGoal, g1) = runRand (uniform wordlist) g0

    -- the whole thing is run over `MonadRandom`, like `Rand g`.  This
    -- allows anyone to grab a "random word" from thin air using `effect`
    -- or `arrM`.
    hangmanRandom :: MonadRandom m => Auto m String (Maybe String)
    hangmanRandom = proc inp -> do

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
        Nothing         -> id -< Just "Unknown command.  @help for help."
        Just Help       -> id -< Just helpmsg
        Just Quit       -> id -< Nothing
        Just (HM hcomm) -> do
          -- Puzzle, with the command. `switchFromF` basically creates
          --   a new "game" with a new word every time the internal `Auto`
          --   emits a blip containing a new word.  the initial game is
          --   `game wordlist firstgoal`.
          puzz   <- switchFromF (game wordlist)
                                (game wordlist firstGoal) -< hcomm

          -- get wins and losses
          losses <- countB . emitOn isFailure -< puzz
          wins   <- countB . emitOn isSuccess -< puzz

          -- display result
          id      -< Just $ case puzz of
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

-- A single game with a single word.  Takes in commands and outputs
--   `PuzzleOut`s...with a blip stream containing the next mystery word.
--   `switchF` takes this blip and creates a fresh `game` out of it,
--   starting the cycle all over.
game :: MonadRandom m
     => [String]      -- ^ wordlist
     -> String        -- ^ new mystery word
     -> Auto m HMCommand (PuzzleOut, Blip String)
     --        ^          ^          ^
     --        |          |          +-- blip stream signaling new game
     --        |          +-- Output puzzle (or puzzle swap)
     --        |
     --        +-- Hangman command
game wordlist goal = proc comm -> do
    -- get correct guesses, incorrect guesses, and solves
    let (corr, incorr, solve) = case comm of
            Guess c | c `elem` goal -> (Just c , Nothing , False)
                    | otherwise     -> (Nothing, Just c  , False)
            Solve s | s == goal     -> (Nothing, Nothing , True )
                    | otherwise     -> (Nothing, Just '*', False)
            _                       -> (Nothing, Nothing , False)

    -- collect all correct and wrong guesses
    rights <- mappendFrom [' ']        -< maybeToList corr
    wrongs <- reverse <$> accum add [] -< incorr

        -- is it solved?
    let solved = solve || all (`elem` rights) goal

        -- did the player run out of guesses?
        failed = length wrongs > guesses

        -- make status
        status | solved    = Success goal
               | failed    = Failure goal
               | otherwise = InProgress

        -- the puzzle object
        puzz   = Puzzle { puzzleString = map (mask rights) goal
                        , puzzleWrongs = wrongs
                        , puzzleStatus = status
                        }

        -- Just p if there should be a new puzzle (from @new or game over)
        mkNew  = case comm of
                   New -> Just (puzz { puzzleStatus = Failure goal })
                   _   | solved || failed -> Just puzz
                       | otherwise        -> Nothing

    -- emits whenever a new puzzle is desired
    mkNewB <- onJusts -< mkNew

    -- tags each new puzzle with a random string pulled from the word list
    -- sequence :: Monad m => (a, m b) -> m (a, b)
    newPuzzB <- arrMB (\x -> sequence (x, uniform wordlist)) -< mkNewB

        -- newSwap emits a new `Swap` when `newPuzzB` emits
    let newSwapB = uncurry Swap . second blankPuzzle <$> newPuzzB

    -- `swapper` is an Interval that is off a first, and then on (as Just)
    -- as soon as `newSwap` emits with a new swapper.
    swapper  <- hold -< newSwapB

        -- puzzOut: what to display if things are to continue as normal
    let puzzOut = Puzz puzz (isDisplay comm)

    id -< (fromMaybe puzzOut swapper, snd <$> newPuzzB)

  where
    -- add a unique element to a list.  but don't check for uniqueness if
    --   '*' (a bad solve)
    add ws w = case w of
                 Just '*'                -> '*':ws
                 Just c | c `notElem` ws -> c  :ws
                 _                       -> ws

-- Utility function to "seal away" the randomness of an `Auto (RandT g m)
-- a b` into a normal `Auto m a b`, with a given initial seed.  The
-- randomness is no longer accessible from the outside.
sealRandom_ :: (RandomGen g, Monad m)
            => Auto (RandT g m) a b
            -> g -> Auto m a b
sealRandom_ = sealState_ . hoistA (StateT . runRandT)

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

isFailure :: PuzzleOut -> Bool
isFailure (Swap (Puzzle _ _ (Failure _)) _) = True
isFailure _                                 = False

isSuccess :: PuzzleOut -> Bool
isSuccess (Swap (Puzzle _ _ (Success _)) _) = True
isSuccess _                                 = False

isDisplay :: HMCommand -> Bool
isDisplay Display = True
isDisplay _       = False

