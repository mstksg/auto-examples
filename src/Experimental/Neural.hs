-- {-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Auto
import Data.List
import Data.Profunctor
import Data.Serialize
import Data.Traversable
import Debug.Trace
import Linear.Metric
import Linear.V4
import Linear.Vector
import Prelude hiding   ((.), id)
import System.Random

type Neural m i o = Auto m (Either (i, o) i) o
type UNeural m i o = Auto m i o
type TNeural m i o = Auto m (i, o) o

fromU :: Monad m
      => UNeural m i o
      -> Neural m i o
fromU = lmap (fst ||| id)

fromT :: (Monad m, Additive o, Num a)
      => TNeural m i (o a)
      -> Neural m i (o a)
fromT = lmap (id ||| (, zero))

-- fixedWeights :: Monad m => [Double] -> UNeural m Double
-- fixedWeights weights = arr (sum . zipWith (*) weights)

-- testNet :: Monad m => UNeural m [Double]
-- testNet = layer 2 [0.2,0.6,0.2] . layer 3 [0.3,0.7] . layer 2 [0.1,0.5,0.1,0.3]
--   where
--     layer :: Monad m => Int -> [Double] -> UNeural m [Double]
--     layer n = sequenceA . replicate n . fixedWeights

trainNodeFrom :: ( Monad vi
                 , Applicative vi
                 , Metric vi
                 , Additive vi
                 , Traversable vi
                 , Num (vi Double)
                 , Show (vi Double)
                 , Serialize (vi Double)
                 , Monad m
                 )
              => vi Double
              -> Neural m (vi Double) Double
trainNodeFrom = mkState f
  where
    dw     = 0.01
    dwDiag = scaled (pure dw)
    wNudge = 0.1
    f (Left (inp, out)) weights = traceShow weights' (dot inp weights', weights')
      where
        result      = dot inp weights
        resultErr   = (result - out)**2
        weights'    = do
          dwb <- dwDiag
          w   <- weights

          let ws'   = dwb + weights
              resE' = (dot inp ws' - out)**2
              drdw  = (resE' - resultErr) / dw

          return (w - drdw * wNudge)
    f (Right inp) weights = (dot inp weights, weights)

testPoints :: [(V4 Double, Double)]
testPoints = map (\[a,b,c,d] -> (V4 a b c d, dot ws (V4 a b c d)))
           . transpose . map (randoms . mkStdGen)
           $ [25645,45764,1354,75673]
  where
    ws = V4 0.05 0.6 0.2 0.15

asTest :: Monad m => Neural m (vi Double) Double -> Neural m (vi Double) Double
asTest = fmap (**2) . liftA2 (-) (arr (snd ||| const 0))

main :: IO ()
main = mapM_ print $ streamAuto' (asTest (trainNodeFrom (V4 0.25 0.25 0.25 0.25))) (take 1000 $ map Left testPoints)

