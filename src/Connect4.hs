{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- import Control.Auto.Generate
-- import Control.Auto.Run
-- import Control.Monad.IO.Class
-- import Data.Map.Strict          (Map)
-- import Debug.Trace
import Control.Auto hiding         (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Process.Random
import Control.Auto.Switch
import Control.Auto.Time
import Control.Monad hiding        (forM_, mapM_)
import Control.Monad.Fix
import Data.Foldable  (toList)
import Data.Function hiding        ((.), id)
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Ord
import Data.Serialize
import GHC.Generics
import Prelude hiding              ((.), id, mapM_)
import System.Console.ANSI
import System.Random
import qualified Data.Map.Strict   as M

type Board = [[Piece]]
type Player = Piece

data Piece = X | O deriving (Show, Read, Eq, Ord, Generic)

data BoardOut = BoardOut { _boBoard  :: !Board
                         , _boWinner :: !(Maybe (Maybe Player))
                         , _boNext   :: !Player
                         , _boFailed :: !Bool
                         } deriving Generic

data Mode = Select | Game InterfaceType InterfaceType deriving Generic
data InterfaceType = Human | CPURandom | CPUAlphaBeta Bool deriving Generic
data C4Out = GameOut BoardOut | PromptInterf Player

-- type Interface m = BoardOut -> m (Maybe Int)

type Interface m = Auto m (Maybe Int, BoardOut) (Maybe Int)

instance Serialize Piece
instance Serialize BoardOut
instance Serialize Mode
instance Serialize InterfaceType

boardWidth, boardHeight :: Int
boardWidth = 7
boardHeight = 6

emptyBoard :: Board
emptyBoard = replicate boardWidth []

emptyBoardOut :: BoardOut
emptyBoardOut = BoardOut emptyBoard Nothing X False

main :: IO ()
main = putStrLn "hello"
-- main = do
--     g <- getStdGen
--     let a0 = game human (cpuAlphaBeta 8 g)
--     loop a0
--   where
--     loop a = do
--       l <- getLine
--       let Output bout a' = runIdentity (stepAuto a l)
--       clearScreen
--       putStrLn (showOut bout)
--       when (isNothing (_boWinner bout)) $ loop a'

driver :: forall m. MonadFix m => StdGen -> Auto m String (Maybe String)
driver g0 = proc inp -> do
    g    <- stdRands split g0 -< ()
    outp <- switchFromF mode initialize -< (inp, g)
    id   -< Nothing
  where
    mode :: Mode -> Auto m (String, StdGen) (C4Out, Blip Mode)
    mode Select       = selecting
    mode (Game i1 i2) = game (interf i1) (interf i2) <<^ fst

    initialize = proc _ -> do
      toSelect <- immediately -< Select
      id -< (undefined, toSelect)
      -- id        -< ("Welcome to Connect 4!", toSelect)

    selecting = undefined

    interf Human = human
    interf CPURandom = cpuRandom undefined
    interf (CPUAlphaBeta hard) | hard      = cpuAlphaBeta 8 undefined
                               | otherwise = cpuAlphaBeta 2 undefined


-- game :: Monad m
--      => String    -- ^ The mystery word(s)
--      -> Auto m (HMCommand, String) (PuzzleOut, Blip String)

game :: forall m. MonadFix m
     => Interface m
     -> Interface m
     -> Auto m String (C4Out, Blip Mode)
game i1 i2 = proc i -> do
    let comm = fmap fst . listToMaybe $ reads i
    bout <- fastForward Nothing game' -< comm
    nextMode <- never -< ()
    id -< (GameOut bout, nextMode)
  where
    game' :: Auto m (Maybe Int) (Maybe BoardOut)
    game' = proc i -> do
      rec bout' <- delay emptyBoardOut -< bout
          move <- mux interf -< (_boNext bout', (i, bout'))
          bout <- board emptyBoard X -< fromMaybe 0 move
      id -< bout <$ move
    interf X = i1
    interf O = i2

human :: Monad m => Interface m
human = arr fst

cpuRandom :: Monad m => StdGen -> Interface m
cpuRandom g = Just <$> stdRands (randomR (1, boardWidth)) g

-- to try out --- some sort of "retry" ?
cpuAlphaBeta :: Monad m => Int -> StdGen -> Interface m
cpuAlphaBeta lim g = proc (_, bout) -> do
    let currP = _boNext bout
        bo0   = _boBoard bout
        a0     = board bo0 currP
        lim'  = min (length (concat bo0) * 2) lim

    weights <- accelerate boardWidth (stdRands random g) -< ()

    let checkOrder = map fst . sortBy (comparing snd)
                   . zip [1 .. boardWidth]
                   $ (weights :: [Double])
        (res, _)   = maxi checkOrder currP lim' BMin BMax a0
        trueRes    = res <|> Just (head checkOrder)

    id -< trueRes
  where
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



-- main :: IO ()
-- main = loop
--   where
--     loop = do
--       if1 <- putStrLn "Player 1 (X):" >> pickInterface
--       if2 <- putStrLn "Player 2 (O):" >> pickInterface
--       playOut if1 if2
--     playOut if1 if2 = do
--       res <- driver if1 if2 emptyBoardOut (board emptyBoard X)
--       clearScreen
--       putStrLn (showOut res)
--       putStrLn "Rematch? (y/q)"
--       l <- getLine
--       case l of
--         'y':_ -> playOut if1 if2
--         _     -> putStrLn "Goodbye!"
--     pickInterface = do
--       putStrLn "1) Human"
--       putStrLn "2) AI Easy"
--       putStrLn "3) AI Medium"
--       putStrLn "4) AI Hard-ish"
--       putStrLn "5) Random player"
--       l <- getLine
--       case l of
--         '1':_ -> return human
--         '2':_ -> return $ cpuAlphaBeta False 2
--         '3':_ -> return $ cpuAlphaBeta False 4
--         '4':_ -> return $ cpuAlphaBeta False 8
--         '5':_ -> return cpuRandom
--         _     -> pickInterface

-- driver :: Interface IO
--        -> Interface IO
--        -> BoardOut
--        -> Auto Identity Int BoardOut
--        -> IO BoardOut
-- driver p1 p2 bout a =
--     case _boWinner bout of
--       Nothing -> do
--         move <- interface bout
--         case move of
--           Just m -> do
--             let Output bout' a' = runIdentity . stepAuto a $ m
--             driver p1 p2 bout' a'
--           Nothing -> do
--             putStrLn "Forfeit!"
--             return bout
--       Just _ ->
--         return bout
--   where
--     interface = case _boNext bout of
--                   X -> p1
--                   O -> p2


board :: MonadFix m => Board -> Player -> Auto m Int BoardOut
board b0 p0 = switchFromF gameOver (board' b0 p0)
  where
    gameOver b = (pure b' &&& id) . never
      where
        b' = b { _boFailed = True }

board' :: MonadFix m => Board -> Player -> Auto m Int (BoardOut, Blip BoardOut)
board' b0 p0 = proc i -> do
    rec currP   <- mkAccum swapP p0 . delay False -< goodMove
        brd     <- toList . fill <$> gather col -< (i, currP)
        lastBrd <- delay b0 -< brd
        let goodMove = lastBrd /= brd

    let winner | isWinner currP brd       = Just (Just currP)
               | length (concat brd) >= d = Just Nothing
               | otherwise                = Nothing
    win <- onJusts -< winner
    let boardOut = BoardOut brd winner (swapP currP goodMove) (not goodMove)
    id -< (boardOut, boardOut <$ win)
  where
    inRange n = n > 0 && n <= length b0
    d         = boardHeight * boardWidth
    fill      = flip M.union (M.fromList (zip [1..] b0))
    col n | inRange n = Just <$> column (b0 !! (n - 1))
          | otherwise = pure Nothing
    swapP p s | s         = opp p
              | otherwise = p

isWinner :: Player -> Board -> Bool
isWinner p b = any (any hasFour) [ filled , transpose filled
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

-- Utilities

showBoard :: Board -> String
showBoard = unlines   . map concat
          . transpose . map fill
  where
    fill :: [Piece] -> [String]
    fill = map (++ "|") . reverse . take boardHeight . (++ repeat "_") . map show

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

column :: Monad m => [Piece] -> Auto m Piece [Piece]
column = mkAccum (\ps p -> take boardHeight (ps ++ [p]))

opp :: Piece -> Piece
opp X = O
opp O = X

-- Interface & AI

-- Ord-to-bound promoter for AI purposes and fast comparisons.
data Bounder a = BMin | BIn a | BMax deriving (Eq, Show)

instance Ord a => Ord (Bounder a) where
    compare BMin BMin = EQ
    compare BMin _    = LT
    compare BMax BMax = EQ
    compare BMax _    = GT
    compare (BIn _) BMin = GT
    compare (BIn _) BMax = LT
    compare (BIn x) (BIn y) = compare x y

