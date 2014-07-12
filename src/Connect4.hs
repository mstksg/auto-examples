{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Auto
import Control.Auto.Collection
import Control.Auto.Blip
import Control.Auto.Switch
import Control.Auto.Generate
import Control.Auto.Run
import Control.Monad
import Data.Foldable
import Data.List hiding          (concat, all, any, and)
import Data.Map.Strict           (Map)
import Data.Serialize
import GHC.Generics
import Prelude hiding            ((.), id, concat, all, any, and)
import qualified Data.Map.Strict as M

type Board = [[Piece]]
type Player = Piece

data Piece = X | O deriving (Show, Read, Eq, Generic)

instance Serialize Piece

boardWidth, boardHeight :: Int
boardWidth = 7
boardHeight = 6

emptyBoard :: Board
emptyBoard = replicate boardWidth []

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

-- -- for testing only
-- addPiece :: Piece -> Int -> Board -> Board
-- addPiece p n = zipWith edit [1..]
--   where
--     edit i r | n == i    = p : r
--              | otherwise = r

showBoard :: Board -> String
showBoard = unlines . map concat
          . transpose . map fill
  where
    fill :: [Piece] -> [String]
    fill = map (++ "|") . reverse . take boardHeight . (++ repeat "_") . map show

showOut :: (Board, Maybe (Maybe Player)) -> String
showOut (b,mmp) = unlines [showBoard b, show mmp]

main :: IO ()
main = void $ interactId (fmap showOut <$> duringRead (board emptyBoard X))
-- main = void $ interactId (fmap show <$> duringRead (column []))

board :: Monad m => Board -> Player -> Auto m Int (Board, Maybe (Maybe Player))
board b0 p0 = switchFromF gameOver (board' b0 p0)
  where
    gameOver (b, w) = (pure (b, Just w) &&& id) . never

-- TODO: feedback if valid move
board' :: Monad m => Board -> Player -> Auto m Int ((Board, Maybe (Maybe Player)), Blip (Board, Maybe Player))
board' b0 p0 = proc i -> do
    currPlayer <- iterator opp p0 -< ()
    brd        <- toList . fill ^<< gather col -< (i, currPlayer)
    let winner | isWinner currPlayer brd  = Just (Just currPlayer)
               | length (concat brd) >= d = Just Nothing
               | otherwise                = Nothing
    win <- onJusts -< winner
    id -< ((brd, winner), (brd,) <$> win)
  where
    d = boardHeight * boardWidth
    fill = M.unionWith (<>) (M.fromList (zip [1..] b0))
    col n | n <= 0 || n > length b0 = pure Nothing
          | otherwise               = Just <$> column (b0 !! (n - 1))
    opp X = O
    opp O = X

column :: Monad m => [Piece] -> Auto m Piece [Piece]
column = mkAccum (\ps p -> take boardHeight (ps ++ [p]))



