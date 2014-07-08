{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

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

data Cell = Dead | Alive
          deriving (Show, Read, Generic)

instance Serialize Cell

type Grid = [[Cell]]

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
                        ,"_|_|*|_|_|_|_|_|*|_|*|_|"
                        ,"_|_|*|_|_|_|_|_|_|*|_|_|"
                        ,"_|_|_|_|_|_|_|_|_|_|_|_|"]

main :: IO ()
main = do
    loop (board startingGrid)
    clearScreen
  where
    loop a = do
      Output g a' <- stepAuto a ()
      clearScreen
      putStrLn (showGrid g)
      _ <- getLine
      () <$ loop a'

board :: MonadFix m => Grid -> Auto m () Grid
board g0 = proc _ -> do
    rec cells <- chunks c ^<< dZipAuto nop cells0 <<^ concat -< neighbors

        let neighborhoods = map ($ cells) allShifts
            neighbors     = map transpose . transpose $ neighborhoods

    id -< cells
  where
    cells0    = concatMap (map cell) g0
    c         = length . head $ g0
    shiftU    = rotateList
    shiftD    = reverse . rotateList . reverse
    shiftL    = map shiftU
    shiftR    = map shiftD
    allShifts = [ shiftU . shiftL , shiftU , shiftU . shiftR
                , shiftR          ,          shiftL
                , shiftD . shiftL , shiftD , shiftD . shiftR ]
    nop       = replicate 2 Alive

cell :: Monad m => Cell -> Auto m [Cell] Cell
cell c0 = switchFromF cell' (cell' c0) <<^ length . filter isAlive
  where
    cell' Alive = (fromBlips Alive &&& id) . tagBlips Dead  . became death
    cell' Dead  = (fromBlips Dead  &&& id) . tagBlips Alive . became spawn

    death, spawn :: Int -> Bool
    death = liftA2 (||) (< 2) (> 3)
    spawn = (== 3)

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

-- utility
rotateList :: [a] -> [a]
rotateList = uncurry (flip (++)) . splitAt 1

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)
