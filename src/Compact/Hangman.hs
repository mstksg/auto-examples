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
import Prelude hiding              ((.), id, mapM_)
import System.Random

{-# ANN Puzzle "HLint: ignore Use String" #-}
{-# ANN game "HLint: ignore Use string literal" #-}

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
    let gameAuto = hangman wordlist g :: Auto Identity String (Maybe String)
    loaded <- try (readAuto savegameFP gameAuto)
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
    let (str, initGame) = stepAuto' loadedGame "@display"
    mapM_ putStrLn str
    finalGame  <- interactId initGame
    putStrLn $ "Saving game to " <> savegameFP <> "..."
    writeAuto savegameFP finalGame
    putStrLn "Goodbye!"

hangman :: Monad m
        => [String]     -- ^ Word list
        -> StdGen       -- ^ Random seed
        -> Auto m String (Maybe String)
hangman wordlist g = proc inp -> do
    let comm = case words inp of
                 "@help"   : _     -> Just Help
                 "@quit"   : _     -> Just Quit
                 "@display": _     -> Just (HM Display)
                 "@solve"  : ws    -> Just . HM . Solve
                                   . map toLower $ unwords ws
                 "@new"    : _     -> Just (HM New)
                 [[c]] | isAlpha c -> Just . HM . Guess $ toLower c
                 _                 -> Nothing

    case comm of
      Nothing         -> id -< return "Unknown command.  @help for help."
      Just Help       -> id -< return helpmsg
      Just Quit       -> id -< mzero
      Just (HM hcomm) -> do
        newstr <- stdRands (pick wordlist) g -< ()
        puzz   <- switchFromF game initialize -< (hcomm, newstr)
        swaps  <- emitOn isSwap              -< puzz
        losses <- countB . filterB isFailure -< swaps
        wins   <- countB . filterB isSuccess -< swaps
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

initialize :: Monad m
           => Auto m (HMCommand, String) (PuzzleOut, Blip String)
initialize = proc (_, newstr) -> do
    new <- immediately -< newstr
    id   -< (Puzz (blankPuzzle newstr) True, new)

game :: Monad m
     => String    -- ^ The mystery word(s)
     -> Auto m (HMCommand, String) (PuzzleOut, Blip String)
game str = proc (comm, newstr) -> do
    let (corr, incorr, solve) = case comm of
            Guess c | c `elem` str -> (Just c , Nothing , False)
                    | otherwise    -> (Nothing, Just c  , False)
            Solve s | s == str     -> (Nothing, Nothing , True )
                    | otherwise    -> (Nothing, Just '*', False)
            _                      -> (Nothing, Nothing , False)
    rights <- accum (++) [' ']         -< maybeToList corr
    wrongs <- reverse <$> accum add [] -< incorr
    let solved = solve || all (`elem` rights) str
        failed = length wrongs > guesses
        status | solved    = Success str
               | failed    = Failure str
               | otherwise = InProgress
        puzz   = Puzzle { puzzleString = map (mask rights) str
                        , puzzleWrongs = wrongs
                        , puzzleStatus = status
                        }
        mkNew  = case comm of
                   New -> Just (puzz { puzzleStatus = Failure str })
                   _   | solved || failed -> Just puzz
                       | otherwise        -> Nothing
    case mkNew of
      Just p' -> do
        let newPuzz = blankPuzzle newstr
        new <- immediately -< newstr
        id   -< (Swap p' newPuzz, new)
      Nothing -> do
        let showScore = isDisplay comm
        new <- never -< ()
        id   -< (Puzz puzz showScore, new)
  where
    add ws w = case w of
                 Just '*'                -> '*':ws
                 Just c | c `notElem` ws -> c  :ws
                 _                       -> ws

pick :: [a] -> StdGen -> (a, StdGen)
pick [] _ = error "pick: Cannot pick from empty list."
pick xs g = (xs !! n, g')
  where
    (n, g') = randomR (0, length xs - 1) g

blankPuzzle :: String -> Puzzle
blankPuzzle str = Puzzle (map (mask []) str) [] InProgress

mask :: String -> Char -> Char
mask _ ' '              = ' '
mask rs c | c `elem` rs = c
          | otherwise   = '_'

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

displayScore :: (Int, Int) -> String
displayScore (w, l) = unwords ["Wins:", show w, "|", "Losses:", show l]

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
