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
                        ,"_|_|_|#|_|_|_|#|#|#|_|_|"
                        ,"_|_|_|_|#|_|_|_|_|_|_|_|"
                        ,"_|_|#|#|#|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|#|_|_|"
                        ,"_|_|#|_|_|_|_|_|#|_|#|_|"
                        ,"_|#|_|#|_|_|_|_|#|_|#|_|"
                        ,"_|_|#|#|_|_|_|_|_|#|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"]

main :: IO ()
main = loop (board startingGrid)
  where
    loop a = do
      Output g a' <- stepAuto a ()
      clearScreen
      putStrLn (showGrid g)
      putStrLn "Press Enter to step simulation."
      _ <- getLine
      () <$ loop a'

board :: forall m. MonadFix m => Grid -> Auto m () Grid
board g0 = proc _ -> do
    rec cells <- chunks c ^<< dZipAuto nop cells0 <<^ concat -< neighbors
        let shiftedGrids = map ($ cells) allShifts
            neighbors    = map transpose . transpose $ shiftedGrids
    id -< cells
  where
    cells0    = concatMap (map cell) g0
    c         = length . head $ g0
    shiftU    = rotateList
    shiftD    = reverse . rotateList . reverse
    shiftL    = map shiftU
    shiftR    = map shiftD
    allShifts = [ shiftU . shiftL , shiftU , shiftU . shiftR , shiftR
                , shiftL , shiftD . shiftL , shiftD , shiftD . shiftR ]
    nop       = replicate 2 Alive

cell :: forall m. Monad m => Cell -> Auto m Neighborhood Cell
cell c0 = switchFromF cell' (cell' c0) <<^ length . filter isAlive
  where
    cell' Alive = (fromBlips Alive &&& id) . tagBlips Dead  . became (\n -> n < 2 || n > 3)
    cell' Dead  = (fromBlips Dead  &&& id) . tagBlips Alive . became (== 3)

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead  = False

showGrid :: Grid -> String
showGrid = unlines . map (concatMap showCell)
  where
    showCell Alive = "#|"
    showCell Dead  = "_|"

readGrid :: [String] -> Grid
readGrid = (map . mapMaybe) readCell
  where
    readCell '|' = Nothing
    readCell '_' = Just Dead
    readCell  _  = Just Alive

rotateList :: [a] -> [a]
rotateList = uncurry (flip (++)) . splitAt 1

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)
