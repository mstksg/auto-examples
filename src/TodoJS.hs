{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}

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
import Control.Auto hiding         (All)
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
import GHCJS.DOM.HTMLAnchorElement
import GHCJS.DOM.HTMLMetaElement
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

data GUI = GUI Filter (Maybe TaskID)

data GUIInp = GIFilter Filter
            | GISelect (Maybe TaskID)

data Filter = All | Active | Completed
            deriving (Show, Generic, Enum, Eq)

instance Serialize Filter

todoAppGUI :: Auto' (Either TodoInp GUIInp) (Map TaskID Task, GUI)
todoAppGUI = proc inp -> do
    outp <- holdWith mempty . perBlip todoApp . emitJusts todoInps -< inp
    filt <- holdWith All                      . emitJusts filtInps -< inp
    selc <- holdWith Nothing                  . emitJusts selcInps -< inp
    id -< (outp, GUI filt selc)
  where
    -- monad comprehensions to act as primitive lenses/filters
    todoInps :: Either TodoInp GUIInp -> Maybe TodoInp
    todoInps etg = [ ti   | Left ti <- Just etg ]
    filtInps :: Either TodoInp GUIInp -> Maybe Filter
    filtInps etg = [ filt | Right (GIFilter filt) <- Just etg ]
    selcInps :: Either TodoInp GUIInp -> Maybe (Maybe TaskID)
    selcInps etg = [ selc | Right (GISelect selc) <- Just etg ]



main :: IO ()
main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView

    (new_todo, main_, footer) <- renderInitial doc

    inputChan <- newChan :: IO (Chan (Either TodoInp GUIInp))
    block <- newEmptyMVar :: IO (MVar ())

    _ <- elementOnkeypress new_todo $ do
      k <- uiKeyCode
      when (k == 13) . liftIO $ do
        inp <- htmlInputElementGetValue new_todo
        unless (null inp) $ do
          writeChan inputChan (Left (IEAdd inp))
          htmlInputElementSetValue new_todo ""

    _ <- renderGui doc main_ footer block inputChan (mempty, GUI All Nothing)

    _ <- runOnChan (renderGui doc main_ footer block inputChan) inputChan todoAppGUI

    takeMVar block

renderInitial :: Document
              -> IO (HTMLInputElement, HTMLElement, HTMLElement)
renderInitial doc = do
    Just hd <- documentGetHead doc

    meta <- createAppend doc hd "meta" castToHTMLMetaElement
    elementSetAttribute meta "charset" "utf-8"

    title <- createAppend doc hd "title" castToHTMLTitleElement
    htmlTitleElementSetText title "auto TodoMVC"

    forM_ ["assets/base.css","assets/index.css"] $ \lnk -> do
      cssLink <- createAppend doc hd "link" castToHTMLLinkElement
      htmlLinkElementSetRel cssLink "stylesheet"
      elementSetAttribute cssLink "charset" "utf-8"
      elementSetAttribute cssLink "type" "text/css"
      htmlLinkElementSetHref cssLink lnk

    Just body <- documentGetBody doc

    todomvc_wrapper <- createAppend doc body "div" castToHTMLDivElement
    elementSetClassName todomvc_wrapper "todomvc-wrapper"

    todoapp <- createAppend doc todomvc_wrapper "section" castToHTMLElement
    elementSetId todoapp "todoapp"

    header <- createAppend doc todoapp "header" castToHTMLElement

    heading <- createAppend doc header "h1" castToHTMLHeadingElement
    htmlElementSetInnerHTML heading "todo"

    new_todo <- createAppend doc header "input" castToHTMLInputElement
    elementSetId new_todo "new-todo"
    htmlInputElementSetPlaceholder new_todo "What needs to be done?"
    htmlInputElementSetAutofocus new_todo True
    htmlInputElementSetName new_todo "newTodo"

    main_ <- createAppend doc todoapp "section" castToHTMLElement
    elementSetId main_ "main"

    footer <- createAppend doc todoapp "footer" castToHTMLElement
    elementSetId footer "footer"

    info <- createAppend doc todomvc_wrapper "footer" castToHTMLElement
    elementSetId info "info"
    htmlElementSetInnerHTML info $ "<p>Double-click to edit a todo</p>"
                                <> "<p>Written by Justin Le</p>"
                                <> "<p>Tempaltes from TodoMVC</p>"

    return (new_todo, main_, footer)


renderGui :: Document
          -> HTMLElement
          -> HTMLElement
          -> MVar ()
          -> Chan (Either TodoInp GUIInp)
          -> (Map TaskID Task, GUI)
          -> IO Bool
renderGui doc main_ footer _ inputChan (tasks, GUI filt selc) = do
    htmlElementSetHidden main_ (M.size tasks == 0)
    htmlElementSetHidden footer (M.size tasks == 0)

    htmlElementSetInnerHTML main_ ""
    htmlElementSetInnerHTML footer ""

    toggle_all <- createAppend doc main_ "input" castToHTMLInputElement
    elementSetAttribute toggle_all "type" "checkbox"
    elementSetId toggle_all "toggle-all"
    htmlInputElementSetName toggle_all "toggle"
    htmlInputElementSetChecked toggle_all allCompleted

    _ <- elementOnclick toggle_all . liftIO $ do
      let newCompl = not allCompleted
      writeChan inputChan (Left (IEAll (TEComplete newCompl)))

    toggle_all_label <- createAppend doc main_ "label" castToHTMLLabelElement
    htmlLabelElementSetHtmlFor toggle_all_label "toggle-all"
    htmlElementSetInnerHTML toggle_all_label "Mark all as complete"

    todo_list <- createAppend doc main_ "ul" castToHTMLUListElement
    elementSetId todo_list "todo-list"

    _ <- M.traverseWithKey (renderTask todo_list) tasks'

    todo_count <- createAppend doc footer "span" castToHTMLElement
    elementSetId todo_count "todo-count"
    htmlElementSetInnerHTML todo_count $ "<strong>"
                                      <> show (M.size uncompl)
                                      <> "</strong> tasks left"

    filters <- createAppend doc footer "ul" castToHTMLUListElement
    elementSetId filters "filters"
    forM_ [All ..] $ \filtType -> do
      filtLi <- createAppend doc filters "li" castToHTMLLIElement

      _ <- elementOnclick filtLi . liftIO $
        writeChan inputChan (Right (GIFilter filtType))

      filtA <- createAppend doc filtLi "a" castToHTMLAnchorElement
      when (filtType == filt) $ elementSetClassName filtA "selected"
      htmlAnchorElementSetText filtA (show filtType)
      htmlAnchorElementSetHref filtA "javascript:void();"


    clear_completed <- createAppend doc footer "button" castToHTMLButtonElement
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
               All       -> tasks
               Active    -> M.filter (not . taskCompleted) tasks
               Completed -> M.filter taskCompleted tasks
    allCompleted = all taskCompleted tasks
    (compl, uncompl) = M.partition taskCompleted tasks

    renderTask :: HTMLUListElement -> TaskID -> Task -> IO ()
    renderTask todo_list tid t = do
        li <- createAppend doc todo_list "li" castToHTMLLIElement
        elementSetClassName li . unwords
          . map snd . filter fst $ [ (taskCompleted t, "completed")
                                   , (selc == Just tid, "editing")
                                   ]

        view <- createAppend doc li "div" castToHTMLDivElement
        elementSetClassName view "view"

        toggle <- createAppend doc view "input" castToHTMLInputElement
        elementSetAttribute toggle "type" "checkbox"
        elementSetClassName toggle "toggle"
        htmlInputElementSetChecked toggle (taskCompleted t)

        _ <- elementOnclick toggle . liftIO $ do
          let newCompl = not (taskCompleted t)
          writeChan inputChan (Left (IETask tid (TEComplete newCompl)))

        descr <- createAppend doc view "label" castToHTMLLabelElement
        htmlElementSetInnerHTML descr (fromMaybe "" (taskDescr t))
        _ <- elementOndblclick descr . liftIO $
          writeChan inputChan (Right (GISelect (Just tid)))

        destroy <- createAppend doc view "button" castToHTMLButtonElement
        elementSetClassName destroy "destroy"

        _ <- elementOnclick destroy . liftIO $
          writeChan inputChan (Left (IETask tid TEDelete))

        edit <- createAppend doc li "input" castToHTMLInputElement
        elementSetClassName edit "edit"
        forM_ (taskDescr t) $ \tdescr ->
          htmlInputElementSetValue edit tdescr
        htmlInputElementSetName edit "title"
        elementSetId edit $ "todo-" <> show tid

        let callback = liftIO $ do
              editString <- htmlInputElementGetValue edit
              if null editString
                then writeChan inputChan (Left (IETask tid TEDelete))
                else do
                  writeChan inputChan (Left (IETask tid (TEModify editString)))
                  writeChan inputChan (Right (GISelect Nothing))

        _ <- elementOnblur edit callback
        _ <- elementOnkeypress edit $ do
          k <- uiKeyCode
          when (k `elem` [13, 27]) callback

        return ()


createAppend :: ( IsDocument self
                , ToJSString tagName
                , IsNode parent
                , IsNode b
                )
             => self
             -> parent
             -> tagName
             -> (Element -> b)
             -> IO b
createAppend doc parent tag coercer = do
    c@(Just child) <- fmap coercer <$> documentCreateElement doc tag
    _ <- nodeAppendChild parent c
    return child
