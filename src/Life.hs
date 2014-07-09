{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Auto hiding     (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Switch
import Control.Monad.Fix
import Data.List
import Data.Maybe
import Data.Serialize
import GHC.Generics
import Prelude hiding          ((.), id)
import System.Console.ANSI

-- Types
data Cell = Dead | Alive
          deriving (Show, Read, Generic)

type Grid = [[Cell]]
type Neighborhood = [Cell]

instance Serialize Cell

-- Starting grid.  A glider, a blinker, a boat, and a beehive.
startingGrid :: Grid
startingGrid = readGrid ["_|_|_|_|_|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"
                        ,"_|_|_|*|_|_|_|*|*|*|_|_|"
                        ,"_|_|_|_|*|_|_|_|_|_|_|_|"
                        ,"_|_|*|*|*|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|*|_|_|"
                        ,"_|_|*|_|_|_|_|_|*|_|*|_|"
                        ,"_|*|_|*|_|_|_|_|*|_|*|_|"
                        ,"_|_|*|*|_|_|_|_|_|*|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"]

main :: IO ()
main = loop (board startingGrid)
  where
    loop a = do
      Output g a' <- stepAuto a ()
      clearScreen
      putStrLn (showGrid g)
      _ <- getLine
      () <$ loop a'

-- the board Auto; takes an initial configuration and returns an
--   automation. (An Auto ignoring its input and just steppin' along.)
board :: forall m. MonadFix m => Grid -> Auto m () Grid
board g0 = proc _ -> do
        -- zipAuto takes a list of Autos and creates a mega Auto that feeds
        --   every input into every internal Auto and collects the output.
        --   Here we zipAuto Autos representing each Cell...and feed a list
        --   containing the neighbors for each cell.  Each cell updates
        --   according to its neighbors, and the output is the updated list
        --   of cells.
        -- 'neighbors' and 'cells' are grids of neighborhoods and
        --   cells...so we use 'concat' to flatten it out and 'chunks c'
        --   to re-chunk it back into a grid.
    rec cells <- chunks c ^<< dZipAuto nop cells0 <<^ concat -< neighbors

            -- a list of every possible "shift" of 'cellGrid'
        let shiftedGrids  :: [Grid]
            shiftedGrids  = map ($ cells) allShifts
            -- going across each Grid in 'shfitedGrids', and accumulating
            --   the cells in every spot.  Basically returns a Grid of
            --   Neighborhoods, where every spot is associated with
            --   a Neighborhood.
            -- Honestly I just found this by typing random things into
            --   ghci until I found something that worked.
            neighbors     :: [[Neighborhood]]
            neighbors     = map transpose . transpose $ shiftedGrids

    id -< cells
  where
    -- the starting list of Cell Autos, to be zipAuto'd
    cells0 :: [Auto m Neighborhood Cell]
    cells0    = concatMap (map cell) g0
    c         = length . head $ g0
    -- Various shifting functions to calculate neighborhoods.
    shiftU    = rotateList
    shiftD    = reverse . rotateList . reverse
    shiftL    = map shiftU
    shiftR    = map shiftD
    allShifts = [ shiftU . shiftL , shiftU , shiftU . shiftR
                , shiftR          ,          shiftL
                , shiftD . shiftL , shiftD , shiftD . shiftR ]
    -- special Neighborhood that keeps a dead cell dead & a live cell live.
    --   Used for default Neighborhood in zipAuto.
    nop :: Neighborhood
    nop       = replicate 2 Alive

-- individual Cell Auto --- give it starting state, and it makes an Auto
--   that takes in a Neighboorhood and returns the next state.
-- switchFromF basically continually runs cell' c0, but every time cell'
--   emits a blip to change its state, restarts cell' with the new state.
cell :: forall m. Monad m => Cell -> Auto m Neighborhood Cell
cell c0 = switchFromF cell' (cell' c0) <<^ length . filter isAlive
  where
    -- Cell Auto that emits its current state and a Blip signaling a state
    --   change.
    cell' :: Cell -> Auto m Int (Cell, Blip Cell)
    cell' Alive = (fromBlips Alive &&& id) . tagBlips Dead  . became death
    cell' Dead  = (fromBlips Dead  &&& id) . tagBlips Alive . became spawn

    -- predicates for swapping
    death, spawn :: Int -> Bool
    death = liftA2 (||) (< 2) (> 3)
    spawn = (== 3)

-- utility
isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead  = False

showGrid :: Grid -> String
showGrid = unlines . map (concatMap showCell)
  where
    showCell Alive = "*|"
    showCell Dead  = "_|"

readGrid :: [String] -> Grid
readGrid = (map . mapMaybe) readCell
  where
    readCell '|' = Nothing
    readCell '*' = Just Alive
    readCell  _  = Just Dead

-- rotateList: [1,2,3,4] -> [2,3,4,1]
rotateList :: [a] -> [a]
rotateList = uncurry (flip (++)) . splitAt 1

-- chunks up items in a list in groups of n.
--       chunks 2 [1,2,3,4,5] -> [[1,2],[3,4],[5]]
chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

