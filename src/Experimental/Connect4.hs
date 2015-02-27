{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Usage:
--
-- $ connect4 [controller X] [controller O]
--
-- Controllers:
--   * h : human
--   * cR: computer (random moves)
--   * cE: computer (easy)
--   * cH: computer (sorta hard)
--
-- Defaults to h vs cH
--

module Main (main) where

import Control.Auto hiding         (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Process.Random
import Control.Auto.Switch
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Data.Foldable               (toList)
import Data.Function hiding        ((.), id)
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Ord
import Data.Serialize
import GHC.Generics
import Prelude hiding              ((.), id, mapM_)
import System.Console.ANSI
import System.Environment
import System.Random
import qualified Data.Map.Strict   as M

-- Types
type Board  = [Column]
type Column = [Piece]
type Player = Piece

-- The Piece type, also representing a player.
data Piece = X | O deriving (Show, Read, Eq, Ord, Generic)

-- The output of the 'Board' Auto that allows the other Autos and stuff to
--   display it and react to it.  Contains the board, the winner (Nothing
--   if no winner, Just (Just p) if p won, and Just Nothing if a tie), the
--   next player up, and if the last move failed.
data BoardOut = BoardOut { _boBoard  :: !Board
                         , _boWinner :: !(Maybe (Maybe Player))
                         , _boNext   :: !Player
                         , _boFailed :: !Bool
                         } deriving Generic

-- The type of the generic controllers (human, CPU, etc).  If the output is
--   Just, it means...that's the move it wants to make.  If the output is
--   Nothing, then the Controller is "asking" for a User input (the Maybe
--   Int).
type Controller m = Auto m (Maybe Int, BoardOut) (Maybe Int)
--                          ^          ^          ^
--                          |          |          +-- Possible output
--                          |          +-- Game output, for decision making
--                          +-- Possible user input

instance Serialize Piece
instance Serialize BoardOut

-- config, and initializations
boardWidth, boardHeight :: Int
boardWidth = 7
boardHeight = 6

emptyBoard :: Board
emptyBoard = replicate boardWidth []

emptyBoardOut :: BoardOut
emptyBoardOut = BoardOut emptyBoard Nothing X False

main :: IO ()
main = do
    -- build the two controllers, from command line arguments.
    args <- getArgs
    (cX, cO) <- case args of
                  []      -> (,) <$> interfOf "h" <*> interfOf "cH"
                  cO:[]   -> (,) <$> interfOf "h" <*> interfOf cO
                  cX:cO:_ -> (,) <$> interfOf cX  <*> interfOf cO

    -- The initial game Auto
    let gameAuto = game cX cO

    -- off we go!
    loop gameAuto 0
  where
    -- Build controllers from command line arguments.
    interfOf :: MonadFix m => String -> IO (Controller m)
    interfOf "cH" = cpuAlphaBeta 8 <$> newStdGen
    interfOf "cE" = cpuAlphaBeta 4 <$> newStdGen
    interfOf "cR" = cpuRandom      <$> newStdGen
    interfOf _    = return human

    -- the main loop
    loop a i0 = do
      let Output bout a' = runIdentity (stepAuto a i0)
      clearScreen
      putStrLn (showOut bout)
      when (isNothing (_boWinner bout)) $ do
        i1 <- fromMaybe 0 . readMaybe <$> getLine
        loop a' i1

-- the main game Auto.  Given two controllers.
--
-- Controllers are just a type alias for a normal Auto:
--
-- > type Controller m = Auto m (Maybe Int, BoardOut) (Maybe Int)
--
-- See the definition of the type for details on what each field means.
game :: forall m. MonadFix m
     => Controller m          -- ^ X Player
     -> Controller m          -- ^ O Player
     -> Auto m Int BoardOut   -- ^ Game Auto
     --        ^   ^
     --        |   +-- Game output
     --        +-- Player input
-- game is the "fastForwarded" @game'@ Auto.
--
-- > fastFoward :: a -> Auto m a (Maybe b) -> Auto m a b
--
-- Basically, 'fastFoward' takes a default value and an Auto returning
--   'Maybe b', and then, whenever that Auto is "run"/ticked, it repeatedly
--   runs the Auto until it gets a 'Just' result.  It is initially run with
--   the input, and then afterwards "fast forwarded" with the default
--   input.  In essense, you get an Auto that "always returns 'Just'"...by
--   "fast fowarding" over the 'Nothing's.  Like TiVo!
--
game cX cO = fastForward Nothing game' <<^ Just
  where
    -- the fast-forwarded game Auto.  It feeds the input directly to the
    --   Controller whose current turn it is, and then outputs a 'Just'
    --   containing the resulting 'BoardOut' whenever an controller
    --   "requests" player input.  Also outputs a 'Just' when the game is
    --   over.
    game' :: Auto m (Maybe Int) (Maybe BoardOut)
          --         ^           ^
          --         |           +-- Board output, if player interaction is
          --         |                 needed, or game is over.
          --         +-- Player interaction, if this is a
          --               "non-fastforwarded" tick.
    game' = proc i -> do
          -- the current BoardOut, bout, is the "last value" of newBout.
          --   We will use this to give to our controllers, so that they
          --   can decide their next moves.
      rec bout     <- lastVal emptyBoardOut -< newBout
          -- feed (i, bout) (the player input, and the current board) to
          --   the player playing next, _boNext bout.
          --
          -- 'mux' is an Auto multiplexer.   Give it an address/key
          --   (_boNext bout, the next player), and it'll feed the input
          --   (i, bout) to the Auto produced by that key, and output the
          --   result.
          --
          -- For example, if you pass in (X, (Just 1, bout)), then it'll
          --   pass in (Just 1, bout) to the X Auto.  'interf' is the
          --   function that maps the key to the Auto, so the X Auto is
          --   'interf X' = 'cX', the X controller.
          move     <- mux interf            -< (_boNext bout, (i, bout))

          -- feed the retrieved move into the Board auto.
          newBout  <- board emptyBoard X    -< fromMaybe 0 move

          -- the output of the Auto.
      let output = case _boWinner newBout of
                     -- if a winner, just report the new BoardOut
                     Just _   -> Just newBout
                     -- If no winner...
                     Nothing -> case move of
                                  -- if a move was received from an
                                  --   Controller, all is good; no need to
                                  --   output anything.  The "fast
                                  --   forwarding" continues.
                                  Just _  -> Nothing
                                  -- if no move is received from an
                                  --   controller, then we need to spark
                                  --   some player interaction.  Return
                                  --   a Just to "halt" the
                                  --   fast-forwarding, and ask for input.
                                  Nothing -> Just newBout

      -- spit out the output.
      id -< output

    -- the correct controller for the player piece.
    interf X = cX
    interf O = cO


-- board: behave like 'board b0 p0' until a 'Blip' is received...then
-- switch permanently to the 'gameOver' Auto.  See the 'hangman' example
-- for more information on 'switchFromF'.
board       :: MonadFix m
            => Board    -- ^ initial Board
            -> Player   -- ^ initial Player
            -> Auto m Int BoardOut
            --        ^   ^
            --        |   +-- Resulting board
            --        +-- Move to make
board b0 p0 = switchFromF gameOver (board' b0 p0)
  where
    -- gameOver: repeatedly just return the finished BoardOut, ignoring any
    -- input...but every move is a failed move (of course).
    gameOver b = (pure b' &&& id) . never
      where
        b' = b { _boFailed = True }

-- The main Board Auto.
board'       :: forall m. MonadFix m
             => Board     -- ^ initial Board
             -> Player    -- ^ initial Player
             -> Auto m Int (BoardOut, Blip BoardOut)
             --        ^    ^         ^
             --        |    |         +-- switch to Game Over, with this
             --        |    |               BoardOut.
             --        |    +-- the resulting Board after the move
             --        +-- the move to make.
board' b0 p0 = proc i -> do

        -- wasGood: whether or not the previously attempted move was legal.
    rec wasGood <- lastVal False                  -< goodMove

        -- currP: the current player.  Will be "swapped" if the last move
        --   was good, and kept the same if it wasn't.
        currP   <- accum swapP p0               -< wasGood

        -- brd: the Board.  uses the 'gather' combinator, which is like the
        --   'mux' combinator, but instead of outputting just the "currently
        --   running/requested" Auto, it outputs the result of every Auto so
        --   far.  Here, it holds every 'column' Auto, and runs the one given
        --   by 'i' (the move).  Gives the 'column' Auto its input, a piece
        --   ('currP').
        --
        -- (toList . fill) fills in the resulting Map as appropriate, and
        --   then turns it back into a list of lists (a 'Board').
        --
        -- It needs to be filled because 'gather' "accumultes new columns",
        --   as you enter in new moves.  So on the first move, if you put
        --   a move in column 4, gather will be a Map with keys [4].  In
        --   the next move, if you put a move in column 2, gather will
        --   return a Map with keys [4,2].  'fill' fills in the gap and
        --   replaces the missing keys/values in the map from the starting
        --   columns in 'b0'.
        brd     <- toList . fill <$> gather col   -< (i, currP)

        -- lastBrd: The previous board, before the new move.
        lastBrd <- lastVal b0                     -< brd

        -- a good move is whether or not the previous board is the same as
        --   the new, updated board (the move was not rejected)
        let goodMove = lastBrd /= brd

    -- the winner.  Just (Just p) if p won, Just Nothing if a tie, and
    --   Nothing if the game is still in progress.
    let winner | isWinner currP brd       = Just (Just currP)
               | length (concat brd) >= d = Just Nothing
               | otherwise                = Nothing

    -- 'win' is a Blip that occurs as soon as 'winner' changes.  'winner'
    --   is initially Nothing, so this basically means, it is emitted
    --   whenever someone wins or there is a tie.
    win <- onChange -< winner

        -- the resulting BoardOut for this tick.
    let boardOut = BoardOut brd
                            winner
                            (swapP currP goodMove)
                            (not goodMove)

    -- pop out the BoardOut, and the 'win', "tagged" with the BoardOut,
    --   when it occurs.
    id -< (boardOut, boardOut <$ win)
  where
    -- if in the right width
    inRange n = n > 0 && n <= length b0
    -- number of tiles on the full board
    d         = boardHeight * boardWidth
    -- fill the resulting map from 'gather' with the missing columns.  See
    -- the binding of 'brd' for more information.
    fill      = flip M.union (M.fromList (zip [1..] b0))

    -- the starting Auto for every column.  If the column number is not in
    --   range, it is an always-Nothing.  If the column is, it's a 'column'
    --   Auto, with starting confirguration given by b0 !! (n - 1).
    --
    -- 'gather' works by "pruning" 'Nothing' results, so the out-of-range
    --   columns are instantly pruned, and the in-range columns are forever
    --   a part of 'gather'.
    col       :: Int -> Auto m Piece (Maybe Column)
    col n     | inRange n = Just <$> column (b0 !! (n - 1))
              | otherwise = pure Nothing
    -- swap the player, if the Bool 's' ("if the last move was good or
    --   not") is True.
    swapP p s | s         = opp p
              | otherwise = p

-- the actual 'column' Auto, of which every 'board' is composed out of.
--   This is a basic usage of 'accum', which is sort of like an Auto
--   foldl.  Give the initial accumulator, and a merging function, apply
--   the function to every incoming item and the accumulator to get a new
--   accumulator and output value.
--
-- In our case, we simply add the new incoming piece to the accumulator (a
--   column), and then 'take' only the amount that we need, keeping the
--   height of the column at most 'boardHeight'.
column :: Monad m => Column -> Auto m Piece Column
column = accum (\ps p -> take boardHeight (ps ++ [p]))


-- Utilities

-- check for winner
isWinner :: Player -> Board -> Bool
isWinner p b = (any . any) hasFour [ filled , transpose filled
                                   , wedgeUp, wedgeDown
                                   ]
  where
    hasFour (j:k:l:m:ns) | and [j,k,l,m] = True
                         | otherwise     = hasFour (k:l:m:ns)
    hasFour _                            = False
    filled    = map (take boardHeight . (++ repeat False) . map (== p)) b
    wedge     = take boardWidth . inits $ repeat False
    wedgeUp   = transpose $ zipWith (++) wedge filled
    wedgeDown = transpose $ zipWith (++) (reverse wedge) filled

-- show the Board
showBoard :: Board -> String
showBoard = unlines   . map concat
          . transpose . map fill
  where
    fill :: [Piece] -> [String]
    fill = map (++ "|") . reverse . take boardHeight . (++ repeat "_") . map show

-- show a BoardOut
showOut :: BoardOut -> String
showOut (BoardOut brd winner nextP _) =
    unlines [ unwords (map show [1..boardWidth])
            , showBoard brd
            , case winner of
                Nothing -> "To play: " ++ show nextP
                Just w  -> "Game over! " ++ case w of
                                              Just p  -> "Winner: " ++ show p
                                              Nothing -> "Tie game."
            ]

-- swap a piece/player.
opp :: Piece -> Piece
opp X = O
opp O = X

-- read, possibly failing with a 'Nothing'.
readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads



-- Controller & AI

-- Ord-to-bound promoter for AI purposes and fast comparisons.
data Bounder a = BMin | BIn a | BMax deriving (Eq, Show, Generic)

instance Ord a => Ord (Bounder a) where
    compare BMin    BMin    = EQ
    compare BMin    _       = LT
    compare BMax    BMax    = EQ
    compare BMax    _       = GT
    compare (BIn _) BMin    = GT
    compare (BIn _) BMax    = LT
    compare (BIn x) (BIn y) = compare x y

instance Serialize a => Serialize (Bounder a)

-- a human controller.  Basically, whatever is received is what is
--   outputted.  Remember that an Controller receives a (Maybe Int,
--   BoardOut), and outputs a (Maybe Int).  So 'arr fst' just echos out
--   that Maybe Int.
--
-- So, when there is user input (Just), echo out that user input.  When
-- there isn't any (Nothing), "request" new input (Nothing).
human :: Monad m => Controller m
human = arr fst

-- A randomized controller.  Ignores its input and outputs Just a random
--   number between 1 and boardWidth at every tick.  Never requires user
--   input.
cpuRandom :: Monad m => StdGen -> Controller m
cpuRandom g = Just <$> stdRands (randomR (1, boardWidth)) g

-- CPU controller with minimax featuring alpha beta pruning.  A somewhat
--   minimal understanding of the minimax + α/β pruning algorithm is
--   assumed :)
--
-- Right now the heuristic isn't too sophisticated.  It rates a victory as
-- +infinity, a loss as -infinity, and neither as 0 ;D
--
-- Implements a "retry" feature: if it sees that the opponent can
--   potentially force a win several moves ahead, it actually re-tries the
--   search with a smaller lookahead.  This is because often times, the
--   algorithm will spot a forced victory before the opponent does...and it
--   would just throw up its hands and give up.  The retry allows it to try
--   again and try to optimize for the short-term instead of the long-term.
cpuAlphaBeta :: MonadFix m
             => Int           -- ^ the suggested lookahead
             -> StdGen        -- ^ shuffling seed
             -> Controller m
cpuAlphaBeta lim g = proc (_, bout) -> do
        -- lastRetry is whether or not the last "tick" resulted in a retry.
    rec lastRetry <- lastVal False -< retry

            -- currP: current (maximizing) player.
        let currP = _boNext bout
            -- bo0: the initial BoardOut
            bo0   = _boBoard bout
            -- a0: the initial board Auto
            a0    = board bo0 currP
            -- lim: the true lookahead limit.  is constrained to 2 if the
            --   last move resulted in a retry, or else the number of
            --   pieces on the board, so we don't waste time doing an
            --   expensive search on the first few moves of the game.
            lim'  | lastRetry = 2
                  | otherwise = min (length (concat bo0) * 2) lim

        -- weights on which to assign potential moves, if it comes down to
        --   a random choice between equally good moves.
        --
        -- stdRands (random g) outputs a new random Double every tick.
        --   'accelerate boardWidth' makes it output 'boardWidth' Doubles
        --   every tick, in a list.
        weights <- accelerate boardWidth (stdRands random g) -< ()

            -- order in which to check the moves.  basically a fisher-yates
            --   shuffle based on 'weights'
        let checkOrder = map fst . sortBy (comparing snd)
                       . zip [1 .. boardWidth]
                       $ (weights :: [Double])
            -- result, goal: the result of the minimax algorithm.  result
            --   is the best move, and goal is the "best we can do if the
            --   opponent plays perfectly" position after that move is
            --   made.
            (res, gl)  = maxi checkOrder currP lim' BMin BMax a0
            -- retry: if this should be a retry.  That is, if the opponent
            --   can force a win --- of gl, the "best we can do if the
            --   opponent plays perfectly", is a loss.
            retry = gl == BMin && not lastRetry
            -- the actual result to output.  If 'res' is Nothing (the maxi
            --   algorithm doesn't find any valid moves), then just output
            --   a random result instead.
            trueRes    = res <|> Just (head checkOrder)

    id -< if retry
            -- if a retry is desired, we output a bogus move that the
            --   'board' auto will reject, so it'll "retry" us for another
            --   reuslt.
            then Just 0
            -- otherwise, here's the move!
            else trueRes
  where
    -- minimax.  Nothing too related to 'Auto' here...mostly just
    --   a not-so-clean implementaiton of minimax w/ alpha beta pruning in
    --   Haskell :)
    maxi :: [Int]                         -- ^ check order
         -> Player                        -- ^ maximizing player
         -> Int                           -- ^ limit
         -> Bounder Double                -- ^ alpha
         -> Bounder Double                -- ^ beta
         -> Auto Identity Int BoardOut    -- ^ board Auto
         -> (Maybe Int, Bounder Double)   -- ^ (best move, score)
    maxi ms maxP l α0 β0 a | l <= 0    = (Nothing, BIn 0)
                           | otherwise = foldr f (Nothing, α0) ms
      where
        f :: Int -> (Maybe Int, Bounder Double) -> (Maybe Int, Bounder Double)
        f m' (m, α) = fromMaybe (m, α) $ do
                        guard . not $ α >= β0
                        guard . not $ _boFailed bout'
                        guard       $ α'' > α
                        return (Just m', α'')
          where
            Output bout' a' = runIdentity (stepAuto a m')
            (_, α')         = mini ms maxP (l - 1) α β0 a'
            α''             = maybe α' (score maxP) $ _boWinner bout'
    mini :: [Int] -> Player -> Int -> Bounder Double -> Bounder Double
         -> Auto Identity Int BoardOut -> (Maybe Int, Bounder Double)
    mini ms maxP l α0 β0 a | l <= 0    = (Nothing, BIn 0)
                           | otherwise = foldr f (Nothing, β0) ms
      where
        f m' (m, β) = fromMaybe (m, β) $ do
                        guard . not $ α0 >= β
                        guard . not $ _boFailed bout'
                        guard       $ β'' < β
                        return (Just m', β'')
          where
            Output bout' a' = runIdentity (stepAuto a m')
            (_, β')         = maxi ms maxP (l - 1) α0 β a'
            β''             = maybe β' (score maxP) $ _boWinner bout'
    score cP (Just p) | p == cP = BMax
                      | otherwise  = BMin
    score _  Nothing  = BIn (-100)
