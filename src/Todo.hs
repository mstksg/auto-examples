{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | "Todo"
--
-- A simple todo app.  Written so that the logic can be plug straight into
-- a GUI...where the GUI would send GUI events as blips into the main
-- `taskInp` Auto, and then re-display the output `Map`.  Right now is
-- equipped with testing functions for command line usage, so there is
-- rudimentary command line usage with the commands shown in
-- `parseInp`...but I expect to port this to various GUI's soon to see how
-- simply it can be done.
--
-- Supports adding, modifying, completing/uncompleting, deleting.

module Todo (TaskID, InpEvent(..), TaskCmd(..), Task(..), todoApp) where

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
data InpEvent = IEAdd  String
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
todoApp :: MonadFix m => Auto m InpEvent (Map TaskID Task)
todoApp = proc inpEvt -> do

        -- the previous taskMap
    rec ids <- arrD M.keys [] -< tMap

        -- "forking" the blip stream
        -- * one blip stream for new task blips, filtering on isAdd
        newTaskB <- perBlip newTask . emitJusts isAdd -< inpEvt
        -- * one blip stream for single task mod blips, filtering on validTE
        modTaskB <- emitJusts validTE                 -< (ids, inpEvt)
        -- * one blip stream for "mass" tasks, `IEAll`
        allTaskB <- emitJusts getMass                 -< (ids, inpEvt)

        -- re-join them back together with `mergeL`
        let singleB :: Blip (TaskID, TaskCmd)
            singleB = newTaskB `mergeL` modTaskB

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
    f _        TEDelete       = Nothing
    f (Just t) (TEComplete c) = Just (t { taskCompleted = c        })
    f (Just t) (TEModify str) = Just (t { taskDescr     = Just str })
    f (Just t) TEPrune        | taskCompleted t = Nothing
                              | otherwise       = Just t
    f t        _              = t

-- | Used with `emitJusts` to filter the stream on "adding" blips
isAdd :: InpEvent -> Maybe String
isAdd (IEAdd descr) = Just descr
isAdd _             = Nothing

