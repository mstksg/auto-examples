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

module Main (main) where

import Control.Applicative
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Interval
import Control.Auto.Process
import Control.Auto.Run
import Control.Auto.Time
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Data.Map                      (Map)
import Data.Maybe
import Data.Monoid
import Data.Serialize
import GHC.Generics
import GHCJS.DOM
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import Prelude hiding                ((.), id)
import Text.Read
import qualified Data.Map            as M

type TaskID = Int

-- | An Input event, from the GUI
data InpEvent = IEAdd  String
              | IETask TaskID TaskCmd
              deriving Show

-- | Describing a task command
data TaskCmd = TEDelete
             | TEComplete Bool
             | TEModify String
             deriving Show

-- | A single task
data Task = Task { taskDescr     :: Maybe String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

instance Serialize Task

-- | The main Auto.  Takes in a Blip stream of input events, and outputs
-- a Map of TaskId's and Tasks.
taskInp :: MonadFix m => Auto m (Blip InpEvent) (Map TaskID Task)
taskInp = proc inpEvtB -> do

        -- the previous taskMap
    rec ids <- arrD M.keys [] -< tMap

        -- "forking" the blip stream
        -- * one blip stream for new task blips, filtering on isAdd
        newTaskB <- perBlip newTask . mapMaybeB isAdd -< inpEvtB
        -- * one blip stream for task mod blips, filtering on validTE
        modTaskB <- mapMaybeB validTE                 -< (ids,) <$> inpEvtB

        -- re-join them back together with `mergeL`
        let tmInpB = newTaskB `mergeL` modTaskB

        -- a Map of Task Id's and Tasks, running `taskMap` per emitted
        -- input
        tMap <- holdWith mempty . perBlip taskMap -< tmInpB

    id -< tMap
  where
    -- Used with `mapMaybeB` to filter the stream on valid task commands
    validTE (ids, IETask n te) | n `elem` ids = Just (n, te)
    validTE _                                 = Nothing

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
taskMap :: Monad m => Auto m (TaskID, TaskCmd) (Map TaskID Task)
taskMap = gather (const taskAuto)
  where
    -- the Auto for each individual task: fold over the folding function
    -- `f` for each input, with the current task.  Use `Nothing` to signal
    -- that it wants to delete itself.
    taskAuto = accum f (Just (Task Nothing False))
    f _        TEDelete       = Nothing
    f (Just t) (TEComplete c) = Just (t { taskCompleted = c        })
    f (Just t) (TEModify str) = Just (t { taskDescr     = Just str })
    f t        _              = t


-- | Used with `mapMaybeB` to filter the stream on adding blips
isAdd :: InpEvent -> Maybe String
isAdd (IEAdd descr) = Just descr
isAdd _             = Nothing

-- | Parse a string input.  Just for testing.  Ideally, these events will
-- come from a GUI.
parseInp :: String -> Maybe InpEvent
parseInp = p . words
  where
    p ("A":xs)   = Just (IEAdd (unwords xs))
    p ("D":n:_)  = readMaybe n <&> \n' -> IETask n' TEDelete
    p ("C":n:_)  = readMaybe n <&> \n' -> IETask n' (TEComplete True)
    p ("U":n:_)  = readMaybe n <&> \n' -> IETask n' (TEComplete False)
    p ("M":n:xs) = readMaybe n <&> \n' -> IETask n' (TEModify (unwords xs))
    p _          = Nothing
    (<&>) :: Functor f => f a -> (a -> b) -> f b
    x <&> f = fmap f x



-- | Just for command line testing use, turning the Map into a String.
-- Ideally this would be handled by a GUI.
formatTodo :: Map TaskID Task -> String
formatTodo = unlines . map format . M.toList
  where
    format (n, Task desc compl) = concat [ show n
                                         , ". ["
                                         , if compl then "X" else " "
                                         , "] "
                                         , fromMaybe "" desc
                                         ]

main :: IO ()
main =
  runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView -- webView.document
    Just body <- documentGetBody doc     -- doc.body

    -- If we are in the browser let's shrink the terminal window to make room
    mbTerminal    <- fmap castToHTMLDivElement   <$> documentGetElementById doc "terminal"
    case mbTerminal of
      Just terminal -> do
        Just style <- elementGetStyle terminal
        cssStyleDeclarationSetProperty style "height" "100" ""
      _             -> return ()

    Just div <- fmap castToHTMLDivElement <$> documentCreateElement doc "div"
    elementSetAttribute div "style" "position:relative;left:0px;top:0px;background-color:#e0d0ff;width:700px;height:500px"
    elementSetAttribute div "id" "todo"
    nodeAppendChild body (Just div)


    -- unlisten <- engine webView "freecell" =<< mkFreecell
    -- unlisten <- undefined

    -- -- Prevent finalizers running too soon
    -- forkIO $ forever (threadDelay 1000000000) >> unlisten

    return ()

