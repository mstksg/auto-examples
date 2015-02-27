{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Collection
import Control.Auto.Generate
import Control.Auto.Process.Random
import Control.Auto.Time
import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector as R
import Data.Array.Repa.Eval as R
import Control.Monad.Fix
import Data.Functor.Identity
import Control.Auto.Core (autoConstr)
import Debug.Trace
import Prelude hiding              ((.), id)
import System.Random

targetDist :: Double -> Double
-- targetDist x = (exp (-((x - 1)**2)) + exp (-((x + 1)**2))) / sqrt (4 * pi)
-- targetDist x = (exp (-((x - 2)**2)) + exp (-((x + 2)**2))) / sqrt (4 * pi)
-- targetDist x = exp (-((x - 2)**2 * 100)) / sqrt (pi / 100)
targetDist x = exp (-((x - 1)**2)) / sqrt pi * 25
{-# INLINE targetDist #-}

-- TODO: adjust dx to match acceptance rate

mhmc :: MonadFix m => Double -> Double -> StdGen -> Auto m a (Double, Int)
mhmc x0 dx g = proc _ -> do
    rec jump <- stdRands (randomR (-dx, dx)) g1 -< ()

        let x'    = x + jump
            p     = targetDist x
            p'    = targetDist x'
            jProb | p' > p    = 1
                  | otherwise = exp (p' - p)

        toJump <- stdRands (randomR (0, 1)) g2 -< ()

        let takeJump = toJump < jProb

        x <- delay_ x0 -< if takeJump then x'
                                      else x

    jumps <- accum_ (+) 0 -< if takeJump then 1 else 0

    id -< (x, jumps)
  where
    (g1, g2) = split g
{-# INLINE mhmc #-}

mhmc' :: Monad m => Double -> Double -> StdGen -> Auto m a (Double, Int)
mhmc' x0 dx g0 = mkState_ (const f) (x0, 0, g0)
  where
    f (x, jC, g) = let (jump, g')    = randomR (-dx, dx) g
                       x'            = x + jump
                       p             = targetDist x
                       p'            = targetDist x'
                       jProb         | p' > p    = 1
                                     | otherwise = exp (p' - p)
                       (toJump, g'') = randomR (0, 1) g'
                       takeJump      = toJump < jProb
                       jC'           | takeJump  = jC + 1
                                     | otherwise = jC
                       x''           | takeJump  = x'
                                     | otherwise = x
                       in  ((x, jC), (x'', jC', g''))
{-# INLINE mhmc' #-}

points, burn, skips, steps :: Int
points = 100
skips = 1000
burn = 10
steps = 10

main :: IO ()
main = do
    -- g <- newStdGen
    -- putStrLn (autoConstr (mhmc 0 0.01 g :: Auto Identity Double (Double, Int)))
    -- putStrLn (autoConstr (mhmc' 0 0.01 g :: Auto Identity Double (Double, Int)))

    a0 <- replicateM points $ do
      g <- newStdGen
      return (head <$> forcer . accelerate skips (fst <$> mhmc 0 0.01 g)) :: IO (Auto' () Double)

    -- let az = zipAuto () a0
    --     xs = runIdentity $ do Output _ burned <- stepN burn az []
    --                           Output xs' _    <- stepAuto (accelerate steps burned) []
    --                           return (last xs')
    -- mapM_ print xs

    let ra :: Array V DIM1 (Auto' () Double)
        ra = R.fromList (Z :. points) a0
        rs :: Array D DIM1 Double
        rs = R.map (runIdentity . flip f ()) ra
        res :: [Double]
        res = R.toList . runIdentity $ computeVectorP rs
        ave = sum res / fromIntegral points
    print ave
    -- mapM_ print res

f :: Monad m => Auto m a b -> a -> m b
f a x = do
    Output _ burned <- stepN burn a x
    Output xs _     <- stepAuto (accelerate steps burned) x
    return (last xs)
{-# INLINE f #-}

-- execAutoN :: Monad m => Int -> Auto m a b -> a -> m (Auto m a b)
-- execAutoN n a x = do
--     Output _ a' <- stepN n a x
--     return a'

-- execAuto :: Monad m => Auto m a b -> a -> m (Auto m a b)
-- execAuto a x = do
--     Output _ a' <- stepAuto a x
--     return a'
-- {-# INLINE execAuto #-}

stepN :: Monad m => Int -> Auto m a b -> a -> m (Output m a b)
stepN 1 a x = stepAuto a x
stepN n a x = do
    Output _ a' <- stepAuto a x
    stepN (n - 1) a' x
{-# INLINE stepN #-}

