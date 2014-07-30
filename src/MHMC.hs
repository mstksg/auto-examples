{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Collection
import Control.Auto.Generate
import Control.Auto.Process.Random
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Data.Functor.Identity
import Debug.Trace
import Prelude hiding              ((.), id)
import System.Random

targetDist :: Double -> Double
-- targetDist x = (exp (-((x - 1)**2)) + exp (-((x + 1)**2))) / sqrt (4 * pi)
-- targetDist x = (exp (-((x - 2)**2)) + exp (-((x + 2)**2))) / sqrt (4 * pi)
-- targetDist x = exp (-((x - 2)**2 * 100)) / sqrt (pi / 100)
targetDist x = exp (-((x - 1)**2)) / sqrt pi * 25

-- TODO: adjust dx to match acceptance rate

mhmc :: MonadFix m => Double -> Double -> StdGen -> Auto m a (Double, Int)
mhmc x0 dx g = proc _ -> do
    rec jump <- rands_ (randomR (-dx, dx)) g1 -< ()

        let x'    = x + jump
            p     = targetDist x
            p'    = targetDist x'
            jProb | p' > p    = 1
                  | otherwise = exp (p' - p)

        toJump <- rands_ (randomR (0, 1)) g2 -< ()

        let takeJump = toJump < jProb

        x <- delay_ x0 -< if takeJump then x'
                                      else x

    jumps <- mkAccum_ (+) 0 -< if takeJump then 1 else 0

    id -< (x, jumps)
  where
    (g1, g2) = split g

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


main :: IO ()
main = do
    a0 <- replicateM 10 $ do
      g <- newStdGen
      return (fst . head <$> forcer . accelerate 1000 (mhmc 0 0.01 g))
    let az = forcer . zipAuto () a0
        Output _ burned = runIdentity . stepN 10 az $ []
        Output xs _ = runIdentity . stepAuto (accelerate 10 burned) $ []
    (mapM_ . mapM_) print xs

stepN :: Monad m => Int -> Auto m a b -> a -> m (Output m a b)
stepN 1 a x = stepAuto a x
stepN n a x = do
    Output _ a' <- stepAuto a x
    stepN (n - 1) a' x

