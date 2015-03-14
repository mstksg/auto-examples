{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}

-- | "Todo-JS"
--
-- Simple todo app on ghcjs, with logic straight from the non-javascript
-- version; that is, an identical 'Auto'.  Mostly a ghcjs wrapper working
-- with GHCJS.DOM ... which is admittedly a little messy, but I'm in the
-- process of cleaning it up :)
--
-- 'Auto' supports adding, modifying, completing/uncompleting, deleting,
-- but so far the GUI only supports adding and deleting.

module Main (main) where

import Control.Applicative
import Control.Auto
import Control.Auto.Run
import Control.Concurrent
import Control.Monad               (unless, when)
import Control.Monad.IO.Class
import Data.Foldable               (forM_, all)
import Data.Map                    (Map)
import Data.Maybe
import Data.Serialize
import GHC.Generics
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.HTMLButtonElement
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLLabelElement
import GHCJS.DOM.HTMLLinkElement
import GHCJS.DOM.HTMLTitleElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import GHCJS.Foreign
import Prelude hiding              ((.), id, all)
import Todo
import qualified Data.Map.Strict   as M

data Filter = Uncompleted | Completed | Both
            deriving (Show, Generic)

instance Serialize Filter

todoAppGUI :: Auto' (Either InpEvent Filter) (Map TaskID Task, Filter)
todoAppGUI = proc inp -> do
    outp <- holdWith mempty . perBlip todoApp . emitJusts justLefts  -< inp
    filt <- holdWith Both                     . emitJusts justRights -< inp
    id -< (outp, filt)
  where
    justRights :: Either a b -> Maybe b
    justRights = either (const Nothing) Just
    justLefts  :: Either a b -> Maybe a
    justLefts  = either Just (const Nothing)



main :: IO ()
main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView -- webView.document

    Just hd <- documentGetHead doc

    Just title <- createCast doc "title" castToHTMLTitleElement
    htmlTitleElementSetText title "auto TodoMVC"
    _ <- nodeAppendChild hd (Just title)

    forM_ ["assets/base.css","assets/index.css"] $ \lnk -> do
      Just cssLink <- createCast doc "link" castToHTMLLinkElement
      _ <- nodeAppendChild hd (Just cssLink)
      htmlLinkElementSetRel cssLink "stylesheet"
      elementSetAttribute cssLink "type" "text/css"
      htmlLinkElementSetHref cssLink lnk

    Just body <- documentGetBody doc

    Just todomvc_wrapper <- createCast doc "div" castToHTMLDivElement
    _ <- nodeAppendChild body (Just todomvc_wrapper)
    elementSetClassName todomvc_wrapper "todomvc-wrapper"
    -- elementSetAttribute todomvc_wrapper "style" "visibility:hidden"

    Just todoapp <- createCast doc "section" castToHTMLElement
    _ <- nodeAppendChild todomvc_wrapper (Just todoapp)
    elementSetId todoapp "todoapp"

    Just header <- createCast doc "header" castToHTMLElement
    _ <- nodeAppendChild todoapp (Just header)

    Just heading <- createCast doc "h1" castToHTMLHeadingElement
    _ <- nodeAppendChild header (Just heading)
    htmlElementSetInnerHTML heading "todo"

    Just new_todo <- createCast doc "input" castToHTMLInputElement
    _ <- nodeAppendChild header (Just new_todo)
    elementSetId new_todo "new-todo"
    htmlInputElementSetPlaceholder new_todo "What needs to be done?"
    htmlInputElementSetAutofocus new_todo True
    htmlInputElementSetName new_todo "newTodo"

    Just main_ <- createCast doc "section" castToHTMLElement
    _ <- nodeAppendChild todoapp (Just main_)
    elementSetId main_ "main"

    Just footer <- createCast doc "footer" castToHTMLElement
    _ <- nodeAppendChild todoapp (Just footer)
    elementSetId footer "footer"

    Just info <- createCast doc "footer" castToHTMLElement
    _ <- nodeAppendChild todomvc_wrapper (Just info)
    elementSetId info "info"
    htmlElementSetInnerHTML info $ "<p>Double-click to edit a todo</p>"
                                <> "<p>Written by Justin Le</p>"
                                <> "<p>Tempaltes from TodoMVC</p>"




    inputChan <- newChan :: IO (Chan (Either InpEvent Filter))
    block <- newEmptyMVar :: IO (MVar ())

    _ <- elementOnkeypress new_todo $ do
      k <- uiKeyCode
      when (k == 13) . liftIO $ do
        inp <- htmlInputElementGetValue new_todo
        unless (null inp) $ do
          writeChan inputChan (Left (IEAdd inp))
          htmlInputElementSetValue new_todo ""

    _ <- renderGui doc main_ footer block inputChan (mempty, Both)

    _ <- runOnChan (renderGui doc main_ footer block inputChan) inputChan todoAppGUI

    takeMVar block

renderGui :: Document
          -> HTMLElement
          -> HTMLElement
          -> MVar ()
          -> Chan (Either InpEvent Filter)
          -> (Map TaskID Task, Filter)
          -> IO Bool
renderGui doc main_ footer _ inputChan (tasks, filt) = do
    print allCompleted

    htmlElementSetHidden main_ (M.size tasks == 0)
    htmlElementSetHidden footer (M.size tasks == 0)

    htmlElementSetInnerHTML main_ ""
    htmlElementSetInnerHTML footer ""

    Just toggle_all <- createCast doc "input" castToHTMLInputElement
    _ <- nodeAppendChild main_ (Just toggle_all)
    elementSetAttribute toggle_all "type" "checkbox"
    elementSetId toggle_all "toggle-all"
    htmlInputElementSetName toggle_all "toggle"
    htmlInputElementSetChecked toggle_all allCompleted

    _ <- elementOnclick toggle_all . liftIO $ do
      let newCompl = not allCompleted
      writeChan inputChan (Left (IEAll (TEComplete newCompl)))

    Just toggle_all_label <- createCast doc "label" castToHTMLLabelElement
    _ <- nodeAppendChild main_ (Just toggle_all_label)
    htmlLabelElementSetHtmlFor toggle_all_label "toggle-all"
    htmlElementSetInnerHTML toggle_all_label "Mark all as complete"

    Just todo_list <- createCast doc "ul" castToHTMLUListElement
    _ <- nodeAppendChild main_ (Just todo_list)
    elementSetId todo_list "todo-list"

    _ <- M.traverseWithKey (renderTask todo_list) tasks'

    Just todo_count <- createCast doc "span" castToHTMLElement
    _ <- nodeAppendChild footer (Just todo_count)
    elementSetId todo_count "todo-count"
    htmlElementSetInnerHTML todo_count $ "<strong>"
                                      <> show (M.size uncompl)
                                      <> "</strong> tasks left"

    Just filters <- createCast doc "ul" castToHTMLUListElement
    _ <- nodeAppendChild footer (Just filters)
    elementSetId filters "filters"
    -- add in filter links here

    Just clear_completed <- createCast doc "button" castToHTMLButtonElement
    _ <- nodeAppendChild footer (Just clear_completed)
    elementSetId clear_completed "clear-completed"
    elementSetClassName clear_completed "clear-completed"
    htmlElementSetHidden clear_completed (M.size compl == 0)
    htmlElementSetInnerHTML clear_completed $ "Clear completed ("
                                           <> show (M.size compl)
                                           <> ")"

    _ <- elementOnclick clear_completed . liftIO $
      writeChan inputChan (Left (IEAll TEPrune))


    return True
  where
    tasks' = case filt of
               Uncompleted -> M.filter (not . taskCompleted) tasks
               Completed   -> M.filter taskCompleted tasks
               Both        -> tasks
    allCompleted = all taskCompleted tasks
    (compl, uncompl) = M.partition taskCompleted tasks

    renderTask :: HTMLUListElement -> TaskID -> Task -> IO ()
    renderTask todo_list tid t = do
        Just li <- createCast doc "li" castToHTMLLIElement
        _ <- nodeAppendChild todo_list (Just li)
        when (taskCompleted t) $
          elementSetClassName li "completed"

        Just view <- createCast doc "div" castToHTMLDivElement
        _ <- nodeAppendChild li (Just view)
        elementSetClassName view "view"

        Just toggle <- createCast doc "input" castToHTMLInputElement
        _ <- nodeAppendChild view (Just toggle)
        elementSetAttribute toggle "type" "checkbox"
        elementSetClassName toggle "toggle"
        htmlInputElementSetChecked toggle (taskCompleted t)

        _ <- elementOnclick toggle . liftIO $ do
          let newCompl = not (taskCompleted t)
          writeChan inputChan (Left (IETask tid (TEComplete newCompl)))

        Just toggle_label <- createCast doc "label" castToHTMLLabelElement
        _ <- nodeAppendChild view (Just toggle_label)
        htmlElementSetInnerHTML toggle_label (fromMaybe "" (taskDescr t))
        -- click handler

        Just destroy <- createCast doc "button" castToHTMLButtonElement
        _ <- nodeAppendChild view (Just destroy)
        elementSetClassName destroy "destroy"

        _ <- elementOnclick destroy . liftIO $
          writeChan inputChan (Left (IETask tid TEDelete))

        return ()


createCast :: (IsDocument self, ToJSString tagName)
           => self -> tagName -> (Element -> b) -> IO (Maybe b)
createCast doc tag coercer = fmap coercer <$> documentCreateElement doc tag
