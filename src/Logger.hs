{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Auto
import Control.Auto.Effects
import Control.Auto.Run
import Control.Auto.Serialize
import Control.Monad
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
    void $ interact id (serializing loggingFP logger)

logger :: MonadIO m
       => Auto m String (Maybe String)
logger = proc input -> do
    let inputwords = words input

    case inputwords of
      -- mzero is Nothing, return is Just
      "@quit":_ -> id -< mzero
      _         -> do
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
        log <- mkAccum (++) "" -< toLog

        -- output the history if requested, or just say "Logged" otherwise.
        id   -< return $ if showHist
                           then displayLog log
                           else "Logged."

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
