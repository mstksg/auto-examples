{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

-- import Control.Auto.Generate
-- import Control.Auto.Run
-- import Control.Monad.IO.Class
-- import Data.Map.Strict        (Map)
-- import Debug.Trace
import Control.Auto hiding       (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Switch
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Function hiding      ((.), id)
import Data.Functor.Identity
import Data.List hiding          (concat, all, any, and, concatMap, notElem, minimum, maximum, maximumBy, foldr)
import Data.Maybe
import Data.Ord
import Data.Serialize
import GHC.Generics
import Prelude hiding            ((.), id, concat, all, any, and, concatMap, notElem, minimum, maximum, foldr)
import System.Console.ANSI
import System.Random
import qualified Data.Map.Strict as M

type Board = [[Piece]]
type Player = Piece

data Piece = X | O deriving (Show, Read, Eq, Generic)

data BoardOut = BoardOut { _boBoard  :: !Board
                         , _boWinner :: !(Maybe (Maybe Player))
                         , _boNext   :: !Player
                         , _boFailed :: !Bool
                         } deriving Generic

type Interface m = BoardOut -> m (Maybe Int)

instance Serialize Piece
instance Serialize BoardOut

data Bounder a = BMin | BIn !a | BMax deriving (Eq, Show)

instance Ord a => Ord (Bounder a) where
    compare BMin BMin = EQ
    compare BMin _    = LT
    compare BMax BMax = EQ
    compare BMax _    = GT
    compare (BIn _) BMin = GT
    compare (BIn _) BMax = LT
    compare (BIn x) (BIn y) = compare x y

boardWidth, boardHeight :: Int
boardWidth = 7
boardHeight = 6

emptyBoard :: Board
emptyBoard = replicate boardWidth []

emptyBoardOut :: BoardOut
emptyBoardOut = BoardOut emptyBoard Nothing X False

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

main :: IO ()
main = do
    if1 <- putStrLn "Player 1 (X):" >> pickInterface
    if2 <- putStrLn "Player 2 (O):" >> pickInterface
    res <- driver if1 if2 emptyBoardOut (board emptyBoard X)
    clearScreen
    putStrLn (showOut res)
  where
    pickInterface = do
      putStrLn "1) Human"
      putStrLn "2) AI Easy"
      putStrLn "3) AI Hard-ish"
      l <- getLine
      case l of
        '1':_ -> return human
        '2':_ -> return $ cpuAlphaBeta False 4
        '3':_ -> return $ cpuAlphaBeta False 8
        _     -> pickInterface

driver :: Interface IO
       -> Interface IO
       -> BoardOut
       -> Auto Identity Int BoardOut
       -> IO BoardOut
driver p1 p2 bout a =
    case _boWinner bout of
      Nothing -> do
        move <- interface bout
        case move of
          Just m -> do
            let Output bout' a' = runIdentity . stepAuto a $ m
            driver p1 p2 bout' a'
          Nothing -> do
            putStrLn "Forfeit!"
            return bout
      Just _ ->
        return bout
  where
    interface = case _boNext bout of
                  X -> p1
                  O -> p2


human :: Interface IO
human bout = do
    clearScreen
    when (_boFailed bout) $ putStrLn "Bad move!"
    putStrLn (showOut bout)
    getOk
  where
    getOk = do
      l <- getLine
      case words l of
        "@quit":_ -> return Nothing
        "@help":_ -> putStrLn "@quit to quit; # to play."
                  >> getOk
        _         -> do
          let res = fmap fst . listToMaybe . reads $ l
          if isNothing res
            then putStrLn "Invalid command.  @help for help."
              >> getOk
            else
              return res

cpuRandom :: Interface IO
cpuRandom _ = Just <$> randomRIO (1, boardWidth)

cpuAlphaBeta :: Bool -> Int -> Interface IO
cpuAlphaBeta dbg lim bout | lim <= 0  = Just <$> randomRIO (1, boardWidth)
                          | otherwise = do
    pickers <- zip [1 .. boardWidth] . randoms <$> newStdGen :: IO [(Int, Double)]
    let checkOrder = map fst . sortBy (comparing snd) $ pickers
        (res, gl)  = maxi checkOrder lim' BMin BMax a0
        trueRes    | isNothing res = Just <$> randomRIO (1, boardWidth)
                   | otherwise     = return res
    case gl of
      BMin  -> do
        putStrLn' "Opponent can force victory."
        cpuAlphaBeta dbg (lim' `div` 2) bout
      BMax  -> do
        putStrLn' "Victory guarunteed."
        trueRes
      BIn s -> do
        putStrLn' $ "Maintaining " ++ show s
        trueRes
  where
    putStrLn' = when dbg . putStrLn
    currP = _boNext bout
    b0    = _boBoard bout
    a0 = board b0 currP
    lim' = min (length (concat b0) * 2) lim
    maxi :: [Int]                         -- check order
         -> Int                           -- limit
         -> Bounder Double                -- alpha
         -> Bounder Double                -- beta
         -> Auto Identity Int BoardOut    -- board Auto
         -> (Maybe Int, Bounder Double)   -- (best move, score)
    maxi ms l α0 β0 a | l <= 0    = (Nothing, BIn 0)
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
            (_, α')         = mini ms (l - 1) α β0 a'
            α''             = fromMaybe α' . fmap score $ _boWinner bout'
    mini :: [Int] -> Int -> Bounder Double -> Bounder Double
         -> Auto Identity Int BoardOut -> (Maybe Int, Bounder Double)
    mini ms l α0 β0 a | l <= 0    = (Nothing, BIn 0)
                      | otherwise = foldr f (Nothing, β0) ms
      where
        f m' (m, β) = fromMaybe (m, β) $ do
                        guard . not $ α0 >= β
                        guard . not $ _boFailed bout'
                        guard       $ β'' < β
                        return (Just m', β'')
          where
            Output bout' a' = runIdentity (stepAuto a m')
            (_, β')         = maxi ms (l - 1) α0 β a'
            β''             = fromMaybe β' . fmap score $ _boWinner bout'
    score (Just p) | p == currP = BMax
                   | otherwise  = BMin
    score Nothing  = BIn (-100)


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

column :: Monad m => [Piece] -> Auto m Piece [Piece]
column = mkAccum (\ps p -> take boardHeight (ps ++ [p]))



opp :: Piece -> Piece
opp X = O
opp O = X
