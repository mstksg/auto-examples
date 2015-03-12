-- {-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Auto
import Debug.Trace
import Data.List
import Data.Profunctor
import System.Random
import Prelude hiding ((.), id)
import Data.Traversable

data Training = Training { trainingInput  :: [Double]
                         , trainingOutput :: [Double]
                         }

type Neural m o = Auto m (Either ([Double], o) [Double]) o
type UNeural m o = Auto m [Double] o
type TNeural m o = Auto m ([Double], o) o

fromU :: Monad m
      => UNeural m o
      -> Neural m o
fromU = lmap (fst ||| id)

fromT :: (Monad m, Monoid o)
      => TNeural m o
      -> Neural m o
fromT = lmap (id ||| (, mempty))

fixedWeights :: Monad m => [Double] -> UNeural m Double
fixedWeights weights = arr (sum . zipWith (*) weights)

testNet :: Monad m => UNeural m [Double]
testNet = layer 2 [0.2,0.6,0.2] . layer 3 [0.3,0.7] . layer 2 [0.1,0.5,0.1,0.3]
  where
    layer :: Monad m => Int -> [Double] -> UNeural m [Double]
    layer n = sequenceA . replicate n . fixedWeights

trainNodeFrom :: Monad m => [Double] -> Neural m Double
trainNodeFrom = mkState f
  where
    dw     = 0.01
    wNudge = 0.1
    -- rNudge = 199
    f (Left (inp, out)) weights = traceShow weights'
                                            (dot inp weights', weights')
      where
        weights'  = map descend (zipWith const [0..] weights)
        result    = dot inp weights
        resultErr = (result - out)**2
        descend i = w - drdw * wNudge
          where
            (wsL, w: wsR) = splitAt i weights
            w'    = w + dw
            ws'   = wsL ++ (w' : wsR)
            resE' = (dot inp ws' - out)**2
            drdw  = (resE' - resultErr) / dw
    f (Right inp) weights = (dot inp weights, weights)

dw :: Double
dw = 0.1

dot :: Num a => [a] -> [a] -> a
dot x y = sum (zipWith (*) x y)

testPoints :: [([Double], Double)]
testPoints = map (\inp -> (inp, dot ws inp)) . transpose . map (randoms . mkStdGen) $ [25645,45764,1354,75673]
  where
    ws = [0.05,0.6,0.2,0.15]
    -- ws = [1,0,0,0]

asTest :: Monad m => Neural m Double -> Neural m Double
asTest = fmap (**2) . liftA2 (-) (arr (snd ||| const 0))

main :: IO ()
main = mapM_ print $ streamAuto' (asTest (trainNodeFrom [0.25,0.25,0.25,0.25])) (take 1000 $ map Left testPoints)
-- main = mapM_ print $ streamAuto' (asTest (trainNodeFrom [1,0,0,0])) (take 1000 $ map Left testPoints)

