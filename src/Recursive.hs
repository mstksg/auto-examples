{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Process.Numerical
import Control.Auto.Time
import Control.Auto.Run
import Control.Monad.Fix
import Prelude hiding ((.), id)

-- The Fibonacci sequence, implemented using 'lastVal'.  In this, `z` is
--   `x + y`, where `x` is `z` delayed by two steps, and `y` is `z` delayed
--   by one step.
--
-- In mathy terms, this means: z_n = z_(n-2) + z_(n-1).
--
-- We output `x`, which is initially `1`.
fib :: MonadFix m => Auto m a Int
fib = proc _ -> do
    rec x <- (delayN 2) 1 -< z      -- z_(n-2)
        y <- (delayN 1) 1 -< z      -- z_(n-1)
        let z = x + y               -- z_n
    id -< x

-- An exponential series: powers of 2.  `x + x` is fed to a delayed version
--   of itself.  That is, `x` starts at 1; the next value is `1 + 1`, 2;
--   the next value is `2 + 2`, 4, etc.
--
-- In mathy terms, this algorithm is basically: z_n = z_(n-1) + z_(n-1).
expo :: MonadFix m => Auto m a Int
expo = proc _ -> do
    rec x <- delay 1 -< x + x       -- z_n = z_(n-1) + z_(n-1)
    id -< x

-- An example from real life.  A `piLoop` is a feedback controller.  It
--   tries to get a "machine" to output a "goal", by feeding it a "control"
--   value.  The control value (like, say, an electric current) goes to the
--   machine (for example, a heating unit) to produce the "response" (for
--   example, a temperature reading).  The goal is to find the appropriate
--   control value to create the desired response.
--
-- In the "PI" algorithm, the control is adjusted at every step by adding
--   together a number proportional to the current error (the "p") and
--   a number proportional to the sum of all errors "so far" (the "i",
--   integral).  We define "error" as the difference between the goal
--   response and the current response.
--
-- Note the use of `summerD`, instead of `summer`.  This means that the
--   "first result" will simply be the initial accumulator value, `c0`.  It
--   is this fixed first-result that allows the knot-tying and
--   fixpoint-finding magic to work.  In a recursive block, there has to be
--   at least *one* value, somewhere, that doesn't depend on anything "now"
--   to get its first output.  `summerD` is that key in this situation, as
--   its first output does not depend on anything else.
--
-- Note that this "key" doesn't have to necessarily be for `currResponse`;
--   you can also move this key value somewhere else:
--
-- > control      <- summer c0         -< p + i
-- > currResponse <- system . delay c0 -< control
--
-- `delay c0` is an `Auto` that outputs `c0` first...then the delayed
--   stream of its inputs.  So the first value of `currResponse` doesn't
--   depend on anything else; it`s just `system` fed `c0`.
--
-- You can have either `control` or `currResponse` be your key value/base
--   case, and it should work the same.  However, if you have neither, then
--   you're going to be going into a `<<loop>>`.
piLoop :: MonadFix m
       => Double                -- ^ initial starting control
       -> Double                -- ^ proportional scaling factor
       -> Double                -- ^ integral scaling factor
       -> Auto m Double Double
piLoop c0 kp ki = proc goal -> do
            -- the error: the goal response, minus the current response
    rec let err = goal - currResponse

        -- the sum of all the errors so far
        errIntegral  <- summer 0   -< err

            -- the p term is proportional to the error
        let p = kp * err
            -- the i term is proportional to the integral
            i = ki * errIntegral

        -- the new control value, the accumulated sum of the p and i terms
        control      <- summerD c0 -< p + i

        -- currResponse, the response of the control applied to the system
        currResponse <- system     -< control

    id -< currResponse
  where
    -- a simple "black box" system...just fancy math and stuff.
    system = arr (\x -> exp x + sin x * 10 + cosh x * 5)

main :: IO ()
main = do
    putStrLn ">>> Fibs!"
    print . fst . stepAutoN' 20 fib $ ()
    putStrLn ">>> Expo!"
    print . fst . stepAutoN' 20 expo $ ()
    putStrLn ">>> PID!"
    print . fst . stepAutoN' 20 (round <$> piLoop 0 0.01 0.005) $ 100
    putStrLn "look at that convergence! beautiful!"
