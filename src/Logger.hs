{-# LANGUAGE Arrows #-}

module Main (main) where

-- | Logger
--
-- Mostly used to demonstrate "automatic serialization".  Using the
-- `serializing` combinator, we transform a normal auto representing
-- a logging process into an auto that automatically, implicitly, and
-- constantly serializes itself...and automatically re-loads the saved
-- state on the program initialization.
--
-- Demonstrates also `resetFrom`, which is a basic switcher that allows an
-- `Auto` to "reset" itself through an output blip stream.
--
-- Also heavy usage of "blip stream" logic and intervals to sort out and
-- manage the stream of inputs into streams that do things and create
-- outputs.

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Core      (unserialize)
import Control.Auto.Interval
import Control.Auto.Run
import Control.Auto.Serialize
import Control.Auto.Switch
import Control.Monad          (void)
import Data.Time
import Data.Traversable       (mapM)
import Prelude hiding         (id, (.), log, mapM)
import System.Locale

-- Commands that can be sent into our logger
data LogCmd = CHistory
            | CQuit
            | CClear
            | CLog String
            deriving (Show, Eq)

-- Saving filepath; could be taken from stdin too.
loggingFP :: FilePath
loggingFP = "data/save/logger"

main :: IO ()
main = do
    putStrLn "<< @history for history >>"
    putStrLn "<< @quit to quit >>"
    putStrLn "<< @clear to clear >>"
    putStrLn "<< type anything to log >>"

    -- run the self-serializing `loggerReset` `Auto`, with the given
    -- initial input functions and processing functions
    void . run getInp processOut $ serializing' loggingFP loggerReset
  where
    getInp     = liftA2 (,) getLine getCurrentTime
    processOut = mapM $ \str -> putStrLn str *> getInp


-- loggerReset wraps around `logger` --- listens on the blip stream coming
--   from `logger`, and resets logger when it emits
loggerReset :: Monad m => Interval m (String, UTCTime) String
loggerReset = resetFrom logger

-- logger auto.  Takes in strings to log, or commands.  Outputs a `Maybe
--   String`, with `Nothing` when it's "done"/quitting.  Also outputs
--   a 'Blip' that tells 'loggerSwitch' to swap out for a fresh logger
--   auto.
logger :: Monad m
       => Auto m (String, UTCTime) (Maybe String, Blip ())
--               ^        ^         ^             ^
--               |        |         |             +-- tell `loggerReset` to
--               |        |         |                 reset `logger`
--               |        |         +-- Command line output.  Nothing means quit
--               |        +-- Time of the command
--               +-- Command line input
logger = proc (input, time) -> do
        -- primitive command parser
    let cmd = case words input of
                "@history":_ -> CHistory
                "@quit":_    -> CQuit
                "@clear":_   -> CClear
                _            -> CLog input

    -- forking the "command" stream into four different blip streams that
    --   emit when the command matches their respective streams.
    -- Note that for the first three we don't even care what the emitted
    --   values are...just *when* they are emitted.
    histB  <- emitOn (== CHistory) -< cmd
    quitB  <- emitOn (== CQuit)    -< cmd
    clearB <- emitOn (== CClear)   -< cmd
    logB   <- emitJusts getLogCmd  -< cmd

    -- accumulate the log when `logB` emits, with a logging string.  apply
    --   `formatLog time` to the emitted value, first.
    log    <- scanB (++) []        -< formatLog time <$> logB

        -- `outputB` is a blip stream that emits when any of these three
        --   streams emit, with the values we are "tagging"/replacing the
        --   streams with with `(<$)`.
    let outputB = mergeLs [ displayLog log <$ histB
                          ,      "Logged." <$ logB
                          ,     "Cleared!" <$ clearB
                          ]

    -- the actual output message will be the last seen message from
    --   `outputB`.
    outputMsg <- holdWith "" -< outputB

    -- output will be `Just outputMsg`, until `quitB` emits.
    -- we "unserialize" before, because we want the whole thing to start
    --   over when we reload/resume the program.  Alternatively, we can
    --   also do:
    --
    --   output <- between -< (outputMsg, (outputB, quitB))
    --
    --   so that the output is "turned back on" whenever `outputB` emits.
    --
    -- output :: Maybe String
    output    <- unserialize before     -< (outputMsg, quitB)

    id -< (output, () <$ clearB)

  where
    -- get a LogCmd's string, if there is one.  Used for `emitJusts`.
    getLogCmd :: LogCmd -> Maybe String
    getLogCmd (CLog str) = Just str
    getLogCmd _          = Nothing

    formatLog :: UTCTime -> String -> [String]
    formatLog time str = [format time <> " " <> str]

    format :: UTCTime -> String
    format = formatTime defaultTimeLocale "<%c>"


-- "pretty print" the log
displayLog :: [String] -> String
displayLog log = "Log length: " <> show loglength
              <> "\n--------\n"
              <> unlines log
              <>   "--------\n"
              <> "Done."
  where
    loglength = length log
