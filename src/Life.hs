-- {-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Auto
import Data.Serialize
import Control.Auto.Run
import Control.Auto.Blip
import Prelude hiding ((.), id)
import GHC.Generics
import Control.Auto.Switch

data Cell = Dead | Alive
          deriving (Show, Generic)

instance Serialize Cell

main :: IO ()
main = () <$ interactId (fmap show ^<< duringRead (cell Alive))

cell :: Monad m => Cell -> Auto m Int Cell
cell c0 = switchF cell' (cell' c0)
  where
    cell' Alive = (fromBlips Alive &&& id) . fmap (Dead  <$) (became death)
    cell' Dead  = (fromBlips Dead  &&& id) . fmap (Alive <$) (became spawn)
    -- cell' Alive = proc i -> do
    --   deathB <- (Dead <$) ^<< became death  -< i
    --   next   <- fromBlips Alive             -< deathB
    --   id     -< (next, deathB)
    -- cell' Dead  = proc i -> do
      -- spawnB <- (Alive <$) ^<< became spawn -< i
      -- next   <- fromBlips Dead              -< spawnB
      -- id      -< (next, spawnB)

    death = liftA2 (||) (< 2) (> 3)
    spawn = (== 3)


