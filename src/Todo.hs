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

module Todo (TaskID, TodoInp(..), TaskCmd(..), Task(..), todoApp) where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Monad
import Control.Monad.Fix
import Data.Map                (Map)
import Data.Maybe
import Data.Serialize
import GHC.Generics
import Prelude hiding          ((.), id)
import qualified Data.Map      as M

type TaskID = Int

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
data Task = Task { taskDescr     :: Maybe String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

instance Serialize Task

-- | The main Auto.  Takes in a stream of input events, and outputs
-- Maps of TaskId's and Tasks.
todoApp :: MonadFix m => Auto m TodoInp (Map TaskID Task)
todoApp = proc inpEvt -> do

    rec -- the id's of all the tasks currently stored
        --
        -- basically applying `M.key` to `tMap`, like `arr M.key`.  But we
        -- use `arrD` instead of `arr` to provide a fixed point for our
        -- recrusive bindings.  See "Recursive.hs" docs for more info.
        ids <- arrD M.keys [] -< tMap

        -- "forking" the blip stream
        -- * one blip stream for new task blips, filtering on isAdd
        newTaskB <- perBlip newTask . emitJusts getAddEvts -< inpEvt
        -- * one blip stream for single task mod blips, filtering on validTE
        modTaskB <- emitJusts validTE                      -< (ids, inpEvt)
        -- * one blip stream for "mass" tasks, `IEAll`
        allTaskB <- emitJusts getMass                      -< (ids, inpEvt)

        let -- re-join them back together with `mergeL`

            -- merge blip streams for single-task events
            singleB :: Blip (TaskID, TaskCmd)
            singleB = newTaskB `mergeL` modTaskB

            -- merge blip streams for all-task events
            allEvtB :: Blip (Map TaskID TaskCmd)
            allEvtB = allTaskB `mergeL` (uncurry M.singleton <$> singleB)

        -- a Map of Task Id's and Tasks, running `taskMap` per emitted
        -- input
        tMap <- holdWith mempty . perBlip taskMap -< allEvtB

    id -< tMap
  where
    -- Used with `mapMaybeB` to filter the stream on valid task commands
    validTE (ids, IETask n te) | n `elem` ids = Just (n, te)
    validTE _                                 = Nothing
    getMass (ids, IEAll te)    = Just (M.fromList (map (,te) ids))
    getMass _                  = Nothing
    getAddEvts (IEAdd dscr)    = Just edscr
    getAddEvts _               = Nothing


    -- Used to increment id's and create a new task
    newTask = proc descr -> do
      newId <- count -< ()
      id -< (newId, TEModify descr)

-- | An Auto that takes a TaskID and a Task command and updates an internal
-- map of TaskID's to Tasks, using `gather`.  See documentation for help on
-- `gather`.
--
-- Basically, `gather` keeps a `Map` of `k`s to `Interval a b`s (An
-- `Interval a b` is just an `Auto a (Maybe b)`.  Give a `(k, a)` as an
-- input, and it feeds the `a` to the `Auto` at `k`.  Every step, outputs
-- a full map of the last emitted value from every `Auto`.
--
-- Here, it emits, at every step, the full map of all tasks and their
-- statuses.  The internal map is a map of Autos representing each task.
-- We can "update" a task by feeding in a tuple with the Task ID we want to
-- update, and the TaskCmd we want the task auto to receive.
--
-- The Task Auto can "delete itself" by outputting `Nothing`.
taskMap :: Monad m => Auto m (Map TaskID TaskCmd) (Map TaskID Task)
taskMap = gatherMany (const taskAuto)
  where
    -- the Auto for each individual task: fold over the folding function
    -- `f` for each input, with the current task.  Use `Nothing` to signal
    -- that it wants to delete itself.
    taskAuto = accum f (Just (Task Nothing False))
    -- `f` updates our task with incoming commands
    f :: Maybe Task -> TaskCmd -> Maybe Task
    f _        TEDelete       = Nothing
    f (Just t) (TEComplete c) = Just (t { taskCompleted = c        })
    f (Just t) (TEModify str) = Just (t { taskDescr     = Just str })
    f (Just t) TEPrune        | taskCompleted t = Nothing
                              | otherwise       = Just t
    f t        _              = t

