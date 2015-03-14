{-# LANGUAGE TupleSections #-}

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
import Control.Monad               (unless)
import Control.Monad.IO.Class
import Data.Foldable               (forM_)
import Data.Map                    (Map)
import Data.Maybe
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
import Prelude hiding              ((.), id)
import Todo
import qualified Data.Map.Strict   as M

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

    Just body <- documentGetBody doc     -- doc.body

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
    -- -- to force the field?
    -- htmlInputElementSetValue new-todo "blah blah"
    -- register on-input handler here
    -- register on-submit handler here?

    Just main_ <- createCast doc "section" castToHTMLElement
    _ <- nodeAppendChild todoapp (Just main_)
    elementSetId main_ "main"
    -- -- should depend on if there are any elements or not
    -- elementSetAttribute s_main "style" "visibility:hidden"

    Just toggle_all <- createCast doc "input" castToHTMLInputElement
    _ <- nodeAppendChild main_ (Just toggle_all)
    elementSetId toggle_all "toggle-all"
    htmlInputElementSetName toggle_all "toggle"
    elementSetAttribute toggle_all "type" "checkbox"
    -- to be determined by if all are complete or not
    htmlInputElementSetChecked toggle_all False
    -- register on-click handler here?

    Just toggle_all_label <- createCast doc "label" castToHTMLLabelElement
    _ <- nodeAppendChild main_ (Just toggle_all_label)
    htmlLabelElementSetHtmlFor toggle_all_label "toggle-all"
    htmlElementSetInnerHTML toggle_all_label "Mark all as complete"

    Just todo_list <- createCast doc "ul" castToHTMLUListElement
    _ <- nodeAppendChild main_ (Just todo_list)
    elementSetId todo_list "todo-list"
    -- items go here

    Just footer <- createCast doc "footer" castToHTMLElement
    _ <- nodeAppendChild todoapp (Just footer)
    elementSetId footer "footer"
    -- -- should depend on if there are any elements or not
    -- elementSetAttribute s_main "style" "visibility:hidden"

    Just todo_count <- createCast doc "span" castToHTMLElement
    _ <- nodeAppendChild footer (Just todo_count)
    elementSetId todo_count "todo-count"
    -- put in tasks left
    htmlElementSetInnerHTML todo_count $ "<strong>" <> "</strong> tasks left"

    Just filters <- createCast doc "ul" castToHTMLUListElement
    _ <- nodeAppendChild footer (Just filters)
    elementSetId filters "filters"
    -- add in filter links here

    Just clear_completed <- createCast doc "button" castToHTMLButtonElement
    _ <- nodeAppendChild footer (Just clear_completed)
    elementSetId clear_completed "clear-completed"
    elementSetClassName clear_completed "clear-completed"
    -- -- should depend on if there are any cleared or not
    -- elementSetAttribute s_main "style" "visibility:hidden"
    -- add in click callbacks here
    htmlElementSetInnerHTML todo_count $ "Clear completed (" <> ")"

    Just info <- createCast doc "footer" castToHTMLElement
    _ <- nodeAppendChild todomvc_wrapper (Just info)
    elementSetId info "info"
    htmlElementSetInnerHTML info $ "<p>Double-click to edit a todo</p>"
                                <> "<p>Written by Justin Le</p>"
                                <> "<p>Tempaltes from TodoMVC</p>"

    

    

    

    return ()



--     Just todoNew <- createCast doc "div" castToHTMLDivElement

--     Just newForm <- createCast doc "form" castToHTMLFormElement
--     Just newInp <- createCast doc "input" castToHTMLInputElement
--     Just newBut <- createCast doc "button" castToHTMLButtonElement
--     htmlElementSetInnerHTML newBut "add"

--     _ <- nodeAppendChild newForm (Just newInp)
--     _ <- nodeAppendChild newForm (Just newBut)

--     _ <- nodeAppendChild todoNew (Just newForm)

--     _ <- nodeAppendChild todoBox (Just todoNew)

--     Just todoItems <- createCast doc "div" castToHTMLDivElement
--     _ <- nodeAppendChild todoBox (Just todoItems)

--     _ <- nodeAppendChild body (Just todoBox)

--     inputChan <- newChan :: IO (Chan InpEvent)
--     block <- newEmptyMVar :: IO (MVar ())

--     _ <- elementOnsubmit newForm $ do
--       preventDefault
--       liftIO $ do
--         inp <- htmlInputElementGetValue newInp
--         unless (null inp) $
--           writeChan inputChan (IEAdd inp)
--         htmlInputElementSetValue newInp ""

--     _ <- runOnChan (updateDiv doc todoItems block inputChan) inputChan taskInp

--     takeMVar block




updateDiv :: Document -> HTMLDivElement -> MVar () -> Chan InpEvent -> Map TaskID Task -> IO Bool
updateDiv doc todoItems _ inputChan tasks = do
    htmlElementSetInnerHTML todoItems ""

    _ <- flip M.traverseWithKey tasks $ \k t -> do
      Just newItem <- createCast doc "div" castToHTMLDivElement
      htmlElementSetInnerHTML newItem (show t)

      _ <- elementOnclick newItem . liftIO $
        writeChan inputChan (IETask k TEDelete)

      nodeAppendChild todoItems (Just newItem)

    return True

createCast :: (IsDocument self, ToJSString tagName) => self -> tagName -> (Element -> b) -> IO (Maybe b)
createCast doc tag coercer = fmap coercer <$> documentCreateElement doc tag
