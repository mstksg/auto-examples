{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Switch
import Control.Auto.Effects
import Control.Auto.Run
import Control.Auto.Serialize
import Control.Monad
import Control.Auto.Blip
import Control.Monad.IO.Class
import Data.Time
import Prelude hiding         (interact, id, (.), log)
import System.Locale

loggingFP :: FilePath
loggingFP = "data/save/logger"

main :: IO ()
main = do
    putStrLn "<< @history for history >>"
    putStrLn "<< @quit to quit >>"
    putStrLn "<< @clear to clear >>"

    -- interact with 'loggerSwitch', automatically serialized and re-loaded
    -- implicitly to the filepath 'loggingFP'
    void $ interact id (serializing loggingFP loggerSwitch)


-- wrapper around 'logger'.  Basically, listenss for blips from 'logger'
-- and switches it out to a new blank log when it receives the blip.
loggerSwitch :: MonadIO m => Auto m String (Maybe String)
loggerSwitch = switchF (\() -> logger) logger

-- logger auto.  Takes in strings to log, or commands.  Outputs a 'Maybe
-- String', with 'Nothing' when it's "done"/quitting.  Also outputs
-- a 'Blip' that tells 'loggerSwitch' to swap out for a fresh logger auto.
logger :: MonadIO m
       => Auto m String (Maybe String, Blip ())
logger = proc input -> do
    let inputwords = words input

    case inputwords of
      -- mzero is Nothing, return is Just
      "@quit":_  -> do
        clear <- never -< ()
        id     -< (mzero, clear)

      "@clear":_ -> do
        clear <- now -< ()
        id     -< (return "Cleared!", clear)

      _          -> do
        -- Get the time at every step
        time <- effect (liftIO getCurrentTime) -< ()

            -- is this a history request?
        let showHist = case inputwords of
                         "@history":_ -> True
                         _            -> False

            -- what to add to the log
            toLog | showHist  = ""
                  | otherwise = format time <> " "
                             <> input
                             <> "\n"

        -- accumulate the log
        log   <- mkAccum (++) "" -< toLog

        -- do not clear
        clear <- never -< ()

        -- output the history if requested, or just say "Logged" otherwise.
        let output | showHist  = displayLog log
                   | otherwise = "Logged."

        id     -< (return output, clear)

-- "pretty print" the log
displayLog :: String -> String
displayLog log = "Log length: " <> show loglength
              <> "\n--------\n"
              <> log
              <>   "--------\n"
              <> "Done."
  where
    loglength = length (lines log)

format :: UTCTime -> String
format = formatTime defaultTimeLocale "<%c>"
