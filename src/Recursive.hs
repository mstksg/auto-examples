{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Process
import Data.Maybe (fromMaybe)
import Control.Auto.Time
import Control.Auto.Run
import Control.Monad.Fix
import Prelude hiding ((.), id)

-- | The Fibonacci sequence, implemented using `delayN`.  In this, `z` is
-- `x + y`, where `x` is `z` delayed by two steps, and `y` is `z` delayed
-- by one step.
--
-- In mathy terms, this means: z_n = z_(n-2) + z_(n-1).
--
-- We output `x`, which is initially `1`.
--
-- > delayN :: Int -> a -> Auto m a a
--
-- `delayN n x` outputs `x` for `n` of the first steps, then outputs
-- whatever it receives, lagging behind `n` steps.
--
fib :: MonadFix m => Auto m a Int
fib = proc _ -> do
    rec x <- (delayN 2) 1 -< z      -- z_(n-2)
        y <- (delayN 1) 1 -< z      -- z_(n-1)
        let z = x + y               -- z_n
    id -< x

-- | An exponential series: powers of 2.  `x + x` is fed to a delayed
-- version of itself.  That is, `x` starts at 1; the next value is `1 + 1`,
-- 2; the next value is `2 + 2`, 4, etc.
--
-- In mathy terms, this algorithm is basically: z_n = z_(n-1) + z_(n-1).
expo :: MonadFix m => Auto m a Int
expo = proc _ -> do
    rec x <- delay 1 -< x + x       -- z_n = z_(n-1) + z_(n-1)
    id -< x

-- | Real-life example; a "pid" controller is a feedback controller; when
-- you have a black box system with input and output, and you want to get
-- the output to a certain target by varying a control value.  You don't
-- know what control corresponds to what output, so you have to "search"
-- for it.  People use this for things like figuring out how much power to
-- feed to a heater to make the room a given temperature.
--
-- PID works by starting with an initial guess, and at every step,
-- adjusting it slightly.  It adjusts it by the current error (target minus
-- current response), the cumulative error, and the consecutive
-- differences between the errors.
--
-- See http://en.wikipedia.org/wiki/PID_controller
--
-- This algorithm here is implemented by just "defining" these concepts,
-- recursively, and how they are related to each other.  Like a graph or
-- network.  Just...*state* the relationships, and the auto figures out how
-- to make them happen.  No iterative loops, no keeping track of state,
-- etc.
--
-- Note that here we use `sumFromD` for the control, instead of `sumFrom`.
-- That's because for recursive bindings like these, we need at least one
-- `Auto` to be able to give a "initial response" without considering any
-- input...just to "start off" the cycle.  `sumFromD` always outputs its
-- initial accumulator first, so it doesn't need any input to output
-- immediately; it begins the chain of evaluation.  This is an important
-- key to remember when doing any recursive bindings --- you need at least
-- one thing to "start" the chain of evaluation that doesn't depend
-- immediately on anything else.
--
-- Anyways, here we represent a system as `System`, an `Auto` that takes
-- stream of `Double`s as input and transforms it into a stream of
-- `Double`s as output.  The `m` means that a `System IO` might do IO in
-- the process of creating its ouputs, for instance.
--
type System m = Auto m Double Double

pid :: MonadFix m => Double -> (Double, Double, Double) -> System m -> System m
pid c0 (kp, ki, kd) blackbox = proc target -> do
    rec --  err :: Double
        --  the difference of the response from the target
        let err        = target - response

        -- cumulativeSum :: Double
        -- the cumulative sum of the errs
        cumulativeSum <- sumFrom 0 -< err

        -- changes :: Maybe Double
        -- the consecutive differences of the errors, with 'Nothing' at first.
        changes       <- deltas    -< err

        --  adjustment :: Double
        --  the adjustment term, from the PID algorithm
        let adjustment = kp * err
                       + ki * cumulativeSum
                       + kd * fromMaybe 0 changes

        -- the control input is the cumulative sum of the adjustments
        -- sumFromD so that it can output its first value immediately (c0),
        -- and begin the chain of recursive evaluation.
        control  <- sumFromD c0 -< adjustment

        -- the response of the system, feeding the control into the blackbox
        response <- blackbox   -< control

    id -< response

main :: IO ()
main = do
    putStrLn ">>> Fibs!"
    print . evalAutoN' 20 fib $ ()
    putStrLn ">>> Expo!"
    print . evalAutoN' 20 expo $ ()
    putStrLn ">>> PID!"
    let blackbox = arr (\x -> exp x + sin x * 10 + cosh x * 5)
    print . evalAutoN' 20 (round' <$> pid 0 (0.01, 0.005, 0.001) blackbox)
                $ 100
    putStrLn "look at that convergence! beautiful!"
  where
    round' :: Double -> Integer
    round' = round


