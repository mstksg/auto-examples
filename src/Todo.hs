{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | "Todo"
--
-- The logic for a simple todo app.  It is structured so that its "input"
-- are commands, and its output is a map of tasks to task id's.
--
-- Hooking it up to a GUI would be as simple as using `runOnChan` from
-- "Control.Auto.Run", and having GUI actions dump commands to the `Chan`
-- queue.
--
-- A simple command line client is in TodoCmd.hs, and a Javascript client
-- using ghcjs can be found in TodoJS.hs.
--
-- Supports adding, modifying, "pruning", completing, uncompleting,
-- deleting single or all tasks at a time.
--
-- Javascript client is online at:
-- http://mstksg.github.io/auto-examples/todo

module Todo (TaskID, TodoInp(..), TaskCmd(..), Task(..), todoApp) where

import Control.Auto
import Control.Auto.Collection
import Control.Monad
import Control.Monad.Fix
import Data.IntMap.Strict           (IntMap, Key)
import Data.Maybe
import Data.Profunctor
import Data.Serialize
import GHC.Generics
import Prelude hiding               ((.), id)
import qualified Data.IntMap.Strict as IM

type TaskID = Key

-- | An Input event, from the GUI
data TodoInp = IEAdd  String
             | IETask TaskID TaskCmd
             | IEAll TaskCmd
             deriving Show

-- | Describing a task command
data TaskCmd = TEDelete
             | TEPrune
             | TEComplete Bool
             | TEModify String
             deriving Show

-- | A single task
data Task = Task { taskDescr     :: String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

instance Serialize Task

-- | The main Auto.  Takes in a stream of input events, and outputs
-- Maps of TaskId's and Tasks.
todoApp :: MonadFix m => Auto m TodoInp (IntMap Task)
todoApp = proc inpEvt -> do

        -- all of the id's of the current stored tasks, in the IntMap
        -- `tmap`.  First result will be `[]`.
    rec allIds <- arrD IM.keys [] -< tMap

        -- "forking" `inpEvt` into three blip streams:
        -- * one blip stream for blips emitting new tasks as strings
        newTaskB <- emitJusts getAddEvts  -< inpEvt
        -- * one blip stream emitting individual targeted commands
        modTaskB <- emitJusts getModEvts  -< inpEvt
        -- * one blip stream emitting "mass" commands
        allTaskB <- emitJusts getMassEvts -< (allIds, inpEvt)

        -- merge the two streams together to get "all" inputs, single and
        -- mass.
        let allInpB = modTaskB <> allTaskB

        -- from a blip stream to an `IntMap` that is empty when the stream
        -- doesn't emit
        allInp <- fromBlips IM.empty -< allInpB

        -- feed the commands and the new tasks to `taskMap`...the result is
        -- the `IntMap` of tasks.
        tMap <- taskMap -< (allInp, newTaskB)

    id -< tMap
  where
    -- blip stream sorters
    getAddEvts :: TodoInp -> Maybe [String]
    getAddEvts (IEAdd descr) = Just [descr]
    getAddEvts _             = Nothing
    getModEvts :: TodoInp -> Maybe (IntMap TaskCmd)
    getModEvts (IETask n te) = Just $ IM.singleton n te
    getModEvts _             = Nothing
    getMassEvts :: ([TaskID], TodoInp) -> Maybe (IntMap TaskCmd)
    getMassEvts (allIds, IEAll te) = Just $ IM.fromList (map (,te) allIds)
    getMassEvts _                  = Nothing


-- | 'Auto' taking an 'IntMap' of task commands, where the key of each
-- command is the ID of the task to send it to.  It also takes a blip
-- stream containing strings for new tasks to create.
--
-- `dynMapF` works to feed the proper command to the proper `taskAuto`, and
-- create new `taskAuto`s on-the-fly with input from the blip stream.
--
-- A task auto can "delete itself" by outputting `Nothing`.
taskMap :: Monad m => Auto m (IntMap TaskCmd, Blip [String]) (IntMap Task)
taskMap = (lmap . first . fmap) Just $ dynMapF taskAuto Nothing
  where
    -- the Auto for each individual task: fold over the folding function
    -- `f` for each input, with the current task.  Use `Nothing` to signal
    -- that it wants to delete itself.
    taskAuto :: Monad m => String -> Interval m (Maybe TaskCmd) Task
    taskAuto descr = accum f (Just (Task descr False))
    -- `f` updates our task with incoming commands; outputting `Nothing`
    -- will end itself.
    f :: Maybe Task -> Maybe TaskCmd -> Maybe Task
    f (Just t) (Just te) = case te of
                             TEDelete     -> Nothing
                             TEComplete c -> Just $ t { taskCompleted = c }
                             TEModify str -> Just $ t { taskDescr = str }
                             TEPrune | taskCompleted t -> Nothing
                                     | otherwise       -> Just t
    f t _                = t
