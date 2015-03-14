{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Auto
import Data.Profunctor
import Data.Serialize
import Data.Traversable
import Debug.Trace
import Linear.Matrix
import Linear.Metric
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Prelude hiding      ((.), id)
import System.Random
import qualified Data.List as L

type Neural m i o = Auto m (Either (i, o) i) o
type UNeural m i o = Auto m i o
type TNeural m i o = Auto m (i, o) o

fromU :: Monad m
      => UNeural m i o
      -> Neural m i o
fromU = lmap (either fst id)

fromT :: (Monad m, Additive o, Num a)
      => TNeural m i (o a)
      -> Neural m i (o a)
fromT = lmap (either id (, zero))

logistic :: Floating a => a -> a -> a -> a
logistic x0 k x = 1 / (1 + exp (-k * (x - x0)))

-- for weights: outer layer is each output, nested/inner layer is the
-- weights for each input.

trainNodeFrom :: forall m vi vo.
                 ( Monad vi
                 , Applicative vi
                 , Metric vi
                 , Additive vi
                 , Traversable vi
                 , Num (vi Double)
                 , Monad vo
                 , Applicative vo
                 , Metric vo
                 , Additive vo
                 , Traversable vo
                 -- , Num (vo Double)
                 , Serialize (vo (vi Double))
                 , Show (vo (vi Double))
                 , Monad m
                 )
              => (vo Double -> vo Double)   -- map before exit
              -> vo (vi Double) -- inner: by-input weights
                                -- outer: by-output weight sets
              -> Neural m (vi Double) (vo Double)
trainNodeFrom outFunc = mkState f
  where
    dw    :: Double
    dw    = 0.05
    wStep :: Double
    wStep = 1
    -- the types work out :|
    nudges :: vo (vi (vo (vi Double)))
    nudges = fmap (outer (scaled (pure dw))) (scaled (pure (pure dw)))
    f :: Either (vi Double, vo Double) (vi Double)
      -> vo (vi Double)
      -> (vo Double, vo (vi Double))
    f (Left (input, expected)) weights =
        -- traceShow weights'
                  (outFunc $ weights' !* input, weights')
      where
        result      = outFunc $ weights !* input
        resultErr   = result `qd` expected
        weights' :: vo (vi Double)
        weights' = do
          nudgeRow <- nudges    :: vo (vi (vo (vi Double)))
          row      <- weights   :: vo (vi Double)
          return $ do
            -- nudgeEl : matrix with a 1 only at the row of this column
            nudgeEl <- nudgeRow :: vi (vo (vi Double))
            weight  <- row      :: vi Double

            let nudged    = weights !+! nudgeEl
                resNudged = outFunc $ nudged !* input
                nudgedErr = resNudged `qd` expected
                dErrdW    = (nudgedErr - resultErr) / dw

            return (weight - dErrdW * wStep)
    f (Right input) weights = (outFunc $ weights !* input, weights)

testPoints :: [(V4 Double, V3 Double)]
testPoints = map (\[a,b,c,d] -> (V4 a b c d, ws !* V4 a b c d))
           . L.transpose . map (randoms . mkStdGen)
           $ [25645,45764,1354,75673]
  where
    -- ws = V1 (V4 0.05 0.6 0.2 0.15)
    ws = V3 (V4 0.05 0.6 0.2 0.15)
            (V4 0    0.1 0.2 0.7 )
            (V4 0.4  0.4 0.1 0.1 )

asTest :: (Additive vo, Monad m)
       => Neural m (vi Double) (vo Double)
       -> Neural m (vi Double) (vo Double)
asTest = liftA2 (^-^) (arr (either snd (const zero)))

testNudge :: V2 (V3 (V2 (V3 Double)))
testNudge = V2 (V3 (V2 (V3 1 0 0)
                       (V3 0 0 0))
                   (V2 (V3 0 1 0)
                       (V3 0 0 0))
                   (V2 (V3 0 0 1)
                       (V3 0 0 0)))
               (V3 (V2 (V3 0 0 0)
                       (V3 1 0 0))
                   (V2 (V3 0 0 0)
                       (V3 0 1 0))
                   (V2 (V3 0 0 0)
                       (V3 0 0 1)))

main :: IO ()
main = mapM_ print $ streamAuto' (quadrance <$> asTest (trainNodeFrom id w0)) (take 1000 $ map Left testPoints)
  where
    -- w0 = V1 (V4 0.25 0.25 0.25 0.25)
    w0 = V3 (V4 0.25 0.25 0.25 0.25)
            (V4 0.25 0.25 0.25 0.25)
            (V4 0.25 0.25 0.25 0.25)

