{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Auto hiding       (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Generate
import Control.Auto.Run
import Control.Auto.Switch
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor.Identity
import Data.List hiding          (concat, all, any, and)
import Data.Map.Strict           (Map)
import Data.Maybe
import Data.Serialize
import Data.Serialize
import GHC.Generics
import Prelude hiding            ((.), id, concat, all, any, and)
import System.Console.ANSI
import System.Random
import qualified Data.Map.Strict as M

type Board = [[Piece]]
type Player = Piece

data Piece = X | O deriving (Show, Read, Eq, Generic)

data BoardOut = BoardOut { _boBoard  :: Board
                         , _boWinner :: Maybe (Maybe Player)
                         , _boNext   :: Player
                         , _boFailed :: Bool
                         } deriving Generic

data InterfaceType = Human | AI

type Interface m = BoardOut -> m (Maybe Int)

instance Serialize Piece
instance Serialize BoardOut

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
    res <- driver human cpuRandom emptyBoardOut (board emptyBoard X)
    putStrLn (showOut res)

-- main = loop (duringRead (board emptyBoard X))
--   where
--     loop a = do
--       inp <- getLine
--       Output b a' <- stepAuto a inp
--       case b of
--         Nothing      -> return ()
--         Just (bo, _) -> do
--           clearScreen
--           putStrLn (showOut bo)
--           loop a'

driver :: Interface IO
       -> Interface IO
       -> BoardOut
       -> Auto Identity Int BoardOut
       -> IO BoardOut
driver p1 p2 bout a = do
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
      Just _ -> do
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
    fmap fst . listToMaybe . reads <$> getLine

cpuRandom :: Interface IO
cpuRandom _ = Just <$> randomRIO (1, boardWidth)



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
    fill      = M.unionWith (<>) (M.fromList (zip [1..] b0))
    col n | inRange n = Just <$> column (b0 !! (n - 1))
          | otherwise = pure Nothing
    opp X = O
    opp O = X
    swapP p s | s         = opp p
              | otherwise = p

column :: Monad m => [Piece] -> Auto m Piece [Piece]
column = mkAccum (\ps p -> take boardHeight (ps ++ [p]))



