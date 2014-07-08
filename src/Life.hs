{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Run
import Control.Auto.Switch
import Control.Auto.Time
import Control.Monad.Fix
import Data.List
import Data.Profunctor
import Data.Serialize
import GHC.Generics
import Prelude hiding          ((.), id)

data Cell = Dead | Alive
          deriving (Show, Read, Generic)

instance Serialize Cell

type Grid = [[Cell]]

gridSize :: Int
gridSize = 4

main :: IO ()
main = () <$ interactId (fmap show ^<< duringRead (board gridSize))
-- main = () <$ interactId (fmap show ^<< duringRead simpleLoop)

-- simpleLoop :: MonadFix m => Auto m () Int
-- simpleLoop = proc _ -> do
--     -- rec x <- counter 1 -< y
--     --     y <- delay 0   -< x
--     rec let a = negate (x `div` 2)
--         v <- counter 0  -< a
--         x <- counter 10 -< v
--       -- let a = 1
--       -- x <- delay 0 . delay 0 . counter 10 . delay 0 . delay 0 -< v
--     id -< x

counter :: Monad m => Int -> Auto m Int Int
counter y0 = mkAuto (counter <$> get)
                    (put y0)
                    $ \x -> Output y0 (counter (y0 + x))

board :: MonadFix m => Int -> Auto m () Grid
board n = proc _ -> do
    rec
      cells <- chunks n
           ^<< zipAuto [] (replicate cellC (cell Dead)) . delay []
           <<^ concat -< dNeighbors
      dNeighbors <- delay [] -< neighbors
      let neighborhoods = allShifts <*> pure cells
          neighbors     = fmap transpose . transpose $ neighborhoods
    id -< cells
  where
    cellC  = n * n
    shiftU = cyc
    shiftD = reverse . cyc . reverse
    shiftL = map shiftU
    shiftR = map shiftD
    allShifts = [ shiftU . shiftL , shiftU , shiftU . shiftR
                , shiftR          ,          shiftL
                , shiftD . shiftL , shiftD , shiftD . shiftR ]




cell :: Monad m => Cell -> Auto m [Cell] Cell
-- cell _ = pure Alive
cell c0 = switchF cell' (cell' c0) <<^ length . filter isAlive
  where
    cell' Alive = (fromBlips Alive &&& id) . tagBlips Dead  . became death
    cell' Dead  = (fromBlips Dead  &&& id) . tagBlips Alive . became spawn

    death = liftA2 (||) (< 2) (> 3)
    spawn = (== 3)

cyc :: [a] -> [a]
cyc = uncurry (flip (++)) . splitAt 1

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead  = False

-- -- lens micro
-- type Iso s t a b = (a -> b) -> s -> t
-- type Iso' s a = (a -> a) -> s -> s

-- iso :: (s -> a) -> (b -> t) -> (a -> b) -> s -> t
-- iso forward backward f = backward . f . forward


-- rot90 :: Iso' Grid Grid
-- rot90 = iso (fmap reverse . transpose) (transpose . fmap reverse)

-- rot180 :: Iso' Grid Grid
-- rot180 = iso (fmap reverse . reverse) (fmap reverse . reverse)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)
