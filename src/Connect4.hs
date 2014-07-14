{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

-- import Control.Auto.Generate
-- import Control.Auto.Run
-- import Control.Monad.IO.Class
-- import Data.Map.Strict        (Map)
import Control.Auto hiding       (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Switch
import Debug.Trace
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Function hiding      ((.), id)
import Data.Functor.Identity
import Data.List hiding          (concat, all, any, and, concatMap, notElem, minimum, maximum)
import Data.Maybe
import Data.Ord
import Data.Serialize
import GHC.Generics
import Prelude hiding            ((.), id, concat, all, any, and, concatMap, notElem, minimum, maximum)
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

-- legalMoves :: Board -> [Int]
-- legalMoves = map fst . filter ((< boardHeight) . length . snd) . zip [1..]

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
    res <- driver human (cpuMiniMax 4) emptyBoardOut (board emptyBoard X)
    putStrLn (showOut res)

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
    -- clearScreen
    when (_boFailed bout) $ putStrLn "Bad move!"

    putStrLn (showOut bout)
    fmap fst . listToMaybe . reads <$> getLine

cpuRandom :: Interface IO
cpuRandom _ = Just <$> randomRIO (1, boardWidth)

cpuMiniMax :: Int -> Interface IO
cpuMiniMax lim bout = do
    print currP
    putStrLn $ showOut bout
    let a0 = board (_boBoard bout) currP
        Output bout1 _ = runIdentity $ stepAuto a0 4
    putStrLn (showOut bout1)
    let res  = maxi lim a0
        choices | null res  = [1 .. boardWidth]
                | otherwise = head . fst <$> res
        numr = length choices - 1
    print res
    p <- randomRIO (0, numr)
    return $ Just (choices !! p)
  where
    currP = _boNext bout
    maxi :: Int -> Auto Identity Int BoardOut -> [([Int], Bounder Double)]
    maxi l a = fromMaybe [] . listToMaybe
             . groupBy ((==) `on` snd)
             . sortBy (comparing snd)
             $ moves
      where
        moves :: [([Int], Bounder Double)]
        moves = do
            m <- [1 .. boardWidth]
            let Output bout' a'   = runIdentity $ stepAuto a m
            guard . not $ _boFailed bout'
            guard $ if _boNext bout' == X then True else error "yo"
            -- guard $ if l >= 2 then traceShow (l, m) (traceShow (_boBoard bout') True)
            --                   else True

            case _boWinner bout' of
              Just r | r == Just currP -> return ([m], BMin)
                     | otherwise       -> return ([m], BIn 90)
              _ -> do
                let minis  = mini (l - 1) a'
                    miniVal | l <= 0 = ([], BIn 0)
                            | null minis = ([], BIn 20)
                            | otherwise            = head minis
                    (mp,mv) = miniVal
                return (m:mp, mv)
    mini :: Int -> Auto Identity Int BoardOut -> [([Int], Bounder Double)]
    mini l a = fromMaybe [] . listToMaybe
             . reverse
             . groupBy ((==) `on` snd)
             . sortBy (comparing snd)
             $ moves
      where
        moves :: [([Int], Bounder Double)]
        moves = do
            m <- [1 .. boardWidth]
            let Output bout' a'   = runIdentity $ stepAuto a m
            guard . not $ _boFailed bout'
            guard $ if _boNext bout' == O then True else error "yo"
            case _boWinner bout' of
              Just r | r == Just (opp currP) -> return ([-m], BMax)
              -- Just r | r == Just (opp currP) -> if False then traceShow ((l, bf, -m)) (trace (showOut bo) (trace (showOut bout') (return ([-m], BIn 90))))
              -- Just r | r == Just (opp currP) -> if l >= 1 then traceShow ((l, bf, -m)) (trace (showOut bo) (trace (showOut bout') (return ([-m], BIn 90))))
              -- Just r | r == Just (opp currP) -> if False then traceShow ((l, -m)) (trace (showOut bout') (return ([-m], BIn 90)))
                                                          -- else return ([-m], BIn 90)
                     | otherwise    -> return ([-m], BIn (-90))
                     -- | otherwise             -> error "hey what is going on mini"
              _ -> do
                let maxis  = maxi (l - 1) a'
                    maxiVal | l <= 0 = ([], BIn 0)
                            | null maxis = ([], BIn 10)
                            | otherwise            = head maxis
                    (mp,mv) = maxiVal
                return ((-m):mp, mv)

            -- let minis = snd <$> mini (l - 1) a'

            -- let minis             = snd <$> mini (l - 1) a'
            --     maxis | l > 0 && not (null minis) = maximum (snd <$> mini (l - 1) a')
            --           | otherwise = BIn 0
            -- case _boWinner bout' of
            --   Just r  -> return (m, min (score r) maxis)
            --   Nothing -> return (m, maxis)
    -- mini :: Int -> Auto Identity Int BoardOut -> [(Int, Bounder Double)]
    -- mini l a = do
    --         m <- [1 .. boardWidth]
    --         let Output bout' a'   = runIdentity $ stepAuto a m
    --             maxis = snd <$> maxi (l - 1) a'
    --             minis | l > 0 && not (null maxis) = minimum (snd <$> maxi (l - 1) a')
    --                   | otherwise = BIn 0
    --         guard . not $ _boFailed bout'
    --         guard . not $ _boWinner bout' == Just Nothing
    --         case _boWinner bout' of
    --           Just r  -> return (m, max (score r) minis)
    --           Nothing -> return (m, minis  )
    -- score (Just w) | w == currP = BMin    -- min = higher priority
    --                | otherwise  = BMax
    -- score Nothing  = BIn 100
    -- maxi :: Int -> Auto Identity Int BoardOut -> [(Int, Maybe (Maybe Player))]
    -- maxi l a | null wins = if l == 0
    --                          then map grab notwins
    --                          else sortBy (\(_,w1) (_,w2) -> sortPri w1 w2) (concatMap getMini notwins)
    --          | otherwise = map grab wins
    --   where
    --     options :: [(Maybe (Maybe Player), (Int, Auto Identity Int BoardOut))]
    --     options = do
    --         m <- [1 .. boardWidth]
    --         let Output bout a' = runIdentity $ stepAuto a m
    --         guard . not $ _boFailed bout
    --         -- guard . not $ _boWinner bout == Just Nothing
    --         return (_boWinner bout, (m, a'))
    --     (wins, notwins) = partition ((== Just (Just n)) . fst) options
    --     getMini (_, (_, a')) = mini (l - 1) a'
    -- mini :: Int -> Auto Identity Int BoardOut -> [(Int, Maybe (Maybe Player))]
    -- mini l a | null losses = if l == 0
    --                            then map grab notlosses
    --                            -- else concatMap getMaxi notlosses
    --                            else sortBy (\(_,w1) (_,w2) -> sortPri w2 w1) (concatMap getMaxi notlosses)
    --          | otherwise   = map grab losses
    --   where
    --     options = do
    --       m <- [1 .. boardWidth]
    --       let Output bout a' = runIdentity $ stepAuto a m
    --       guard . not $ _boFailed bout
    --       return (_boWinner bout, (m, a'))
    --     (losses, notlosses) = partition ((== Just (Just (opp n))) . fst) options
    --     getMaxi (_, (_, a')) = maxi (l - 1) a'
    -- grab (bow, (m, _)) = (m, bow)
    -- sortPri :: Maybe (Maybe Player) -> Maybe (Maybe Player) -> Ordering
    -- sortPri r1 r2 = case (r1, r2) of
    --                   (Just (Just w1), Just (Just w2)) | w1 == n && w2 == n -> EQ
    --                                                    | w1 == n            -> LT
    --                                                    | w2 == n            -> GT
    --                                                    | otherwise          -> EQ
    --                   (Just (Just w1), Just Nothing  ) | w1 == n            -> LT
    --                                                    | otherwise          -> GT
    --                   (Just (Just w1), Nothing       ) | w1 == n            -> LT
    --                                                    | otherwise          -> GT
    --                   (Just Nothing, Just Nothing)     -> EQ
    --                   (Just Nothing, Nothing)          -> GT
    --                   (Nothing, Nothing)               -> EQ
    --                   (_ , _)                          -> compare EQ (sortPri r2 r1)


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
