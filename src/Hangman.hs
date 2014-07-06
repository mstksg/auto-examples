{-# LANGUAGE Arrows #-}

module Main (main) where

-- import Control.Arrow
-- import Control.Auto.Effects
-- import Control.Auto.Event.Internal
-- import Control.Auto.Generate
-- import Control.Auto.Time
-- import Data.Functor
import Control.Auto
import Control.Auto.Event
import Control.Auto.Run
import Control.Auto.Switch
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Prelude hiding                 ((.), id)
import System.Random

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
                     , puzzleStatus  :: Status
                     } deriving Show

data Status = InProgress
            | Success String
            | Failure String
            deriving Show

wordlistFP :: FilePath
wordlistFP = "data/wordlist.txt"

guesses :: Int
guesses = 7

helpmsg :: String
helpmsg = "Solve the hangman!  @new for a new game.  @solve [solution] to "
       <> "solve.  @quit to quit.  Type any single character to guess that"
       <> " character."

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
                 "@help" :_        -> id -< Just Help
                 "@quit" :_        -> id -< Just Quit
                 "@solve":ws       -> id -< Just $ HM (Solve (map toLower (unwords ws)))
                 "@new"  :_        -> do
                     w <- mkState_ (\_ -> pick wordlist) g -< ()
                     id -< Just $ HM (New w)
                 [[c]] | isAlpha c -> id -< Just $ HM (Guess (toLower c))
                 _                 -> id -< Nothing

    case comm of
      Nothing         -> id -< return "Unknown command.  @help for help."
      Just Help       -> id -< return helpmsg
      Just Quit       -> id -< mzero
      Just (HM hcomm) -> do
        puzz <- switchF game (game Nothing) -< hcomm
        id    -< Just $ case puzz of
                          Just p  -> display p
                          Nothing -> "No game!"


game :: Monad m
     => Maybe String
     -> Auto m HMCommand (Maybe Puzzle, Event (Maybe String))
game mstr0 = proc comm ->
    case (mstr0, comm) of
      (_, New str1)  -> do
        let newPuzz = Puzzle (map (mask "") str1) "" InProgress
        new <- now -< Just str1
        id -< (Just newPuzz, new)

      (Nothing, _)   -> do
        e <- never -< ()
        id -< (Nothing, e)

      (Just str, _) -> do
        let (corr, incorr, solve) = case comm of
                Guess c | c `elem` str -> (Just c , Nothing , False)
                        | otherwise    -> (Nothing, Just c  , False)
                Solve s | s == str     -> (Nothing, Nothing , True )
                        | otherwise    -> (Nothing, Just '*', False)
                _                      -> (Nothing, Nothing , False)

        rights <- mkAccum add " "            -< corr

        let solved = solve || all (`elem` rights) str

            wrong  | solved    = Nothing
                   | otherwise = incorr

        wrongs <- reverse <$> mkAccum add "" -< wrong

        let failed = length wrongs > guesses

            status | solved    = Success str
                   | failed    = Failure str
                   | otherwise = InProgress

            puzz   = Puzzle { puzzleString = map (mask rights) str
                            , puzzleMisses = wrongs
                            , puzzleStatus = status
                            }

        new <- never -< ()

        id -< (Just puzz, new)

  where
    add ws w = case w of
                 Just '*'                -> '*':ws
                 Just c | c `notElem` ws -> c  :ws
                 _                       -> ws
    mask _ '_'              = ' '
    mask rs c | c `elem` rs = c
              | otherwise   = '_'

pick :: [a] -> StdGen -> (a, StdGen)
pick [] _ = error "pick: Cannot pick from empty list."
pick xs g = (xs !! n, g')
  where
    (n, g') = randomR (0, length xs - 1) g

display :: Puzzle -> String
display (Puzzle str m sts) = pre
                          <> " ["
                          <> str'
                          <> "] ("
                          <> m
                          <> replicate (guesses + 1 - length m) '.'
                          <> ")"
  where
    (pre, str') = case sts of
                    InProgress -> ("Active:", str)
                    Success s  -> ("Solved!", s  )
                    Failure s  -> ("Failed!", s  )
