{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
-- | "Todo-JS"
--
-- Simple todo app on ghcjs, with logic straight from the non-javascript
-- version; that is, an identical 'Auto'.  Mostly a ghcjs wrapper working
-- with GHCJS.DOM ... which is admittedly a little messy.  All of the todo
-- logic is in `Todo.hs`, so check that out first :)  This is just the
-- "view".
--
-- https://github.com/mstksg/auto-examples/blob/master/src/Todo.hs
--
-- If you're building this, be sure to grab the css asset files from the
-- project directory.
--
-- Still missing persistence to localStorage and routing.  While
-- persistance might be a useful demonstration of implicit serialiation,
-- a part of it (and routing too) might be outside of the range of domain
-- of `auto`...so these aren't really high-priority for now.
--
-- This is compiled and hosted online at:
-- http://mstksg.github.io/auto-examples/todo

module Main (main) where

import Control.Applicative
import Control.Auto hiding          (All)
import Control.Auto.Run
import Control.Concurrent
import Control.Monad                (unless, when, join)
import Control.Monad.IO.Class
import Data.Foldable                (forM_, all)
import Data.IntMap                  (IntMap)
import Data.Maybe
import Data.Serialize
import GHC.Generics
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.HTMLAnchorElement
import GHCJS.DOM.HTMLButtonElement
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLLabelElement
import GHCJS.DOM.HTMLLinkElement
import GHCJS.DOM.HTMLMetaElement
import GHCJS.DOM.HTMLTitleElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import GHCJS.Foreign

import Control.Monad.State (MonadState)
import GHCJS.DOM.Types (GObject, toGObject, unsafeCastGObject)
import GHCJS.DOM.UIEvent
import GHCJS.Marshal
import GHCJS.Types
import VirtualDom
import qualified VirtualDom as VD
import VirtualDom.Prim (text)
import qualified VirtualDom.Prim as VDP
import VirtualDom.HTML.Attributes
import Control.Lens

import Prelude hiding               ((.), id, all)
import Todo
import qualified Data.IntMap.Strict as IM

data GUIOpts = GUI { _currFilter   :: Filter        -- currently applied filter
                   , _currSelected :: Maybe TaskID  -- currently selected task
                   }

data GUIInp = GIFilter Filter
            | GISelect (Maybe TaskID)

data Filter = All | Active | Completed
            deriving (Show, Generic, Enum, Eq)

instance Serialize Filter

-- | A new `Auto` that takes in commands from the GUI (that can either be
-- commands for the Todo app logic itself, or commands to change some GUI
-- option).
--
-- Basically filters the input stream into three different blip streams and
-- recombines them all together in the end.
--
-- The result is a tuple with all of the alive `Task` items, and GUI option
-- settings.
todoAppGUI :: Auto' (Either TodoInp GUIInp) (IntMap Task, GUIOpts)
todoAppGUI = proc inp -> do
    -- process the input items that are for the Todo app itself.  pretty
    --   much just feeds it to the `todoApp` auto, from `Todo.hs`, which
    --   has the actual logic.
    outp <- holdWith mempty . perBlip todoApp . emitJusts todoInps -< inp
    -- `filt` will be the last seen filter setting inputted, starting with
    --   `All`.  It is the currently applied filter.
    filt <- holdWith All                      . emitJusts filtInps -< inp
    -- `selc` will be the last seen selection setting inputted, starting with
    --   `Nothing`.  It is the currently selected/edited task.
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
main = do
    initDomDelegator
    vbody <- newTopLevelContainer

    -- The `Chan` queue to dump all commands triggered by GUI actions
    inputChan <- newChan :: IO (Chan (Either TodoInp GUIInp))

    runWebGUI $ \ webView -> do
      Just doc <- webViewGetDomDocument webView

      -- render the skeleton, giving a reference to the todo list and the
      --   info footer on the DOM
      _ <- renderInitial doc vbody inputChan

      -- Run the `Auto` `todoAppGUI` on the `inputChan` queue, by waiting
      --   for new commands (deposited by the GUI) to show up on the queue,
      --   feeding them through `todoAppGUI`, and "rendering" the output
      --   with `renderGui doc main_ footer inputChan`.
      --
      _ <- runOnChan (renderGui doc vbody inputChan)
                     inputChan
                     todoAppGUI

      return ()

-- | Set up the "static" skeleton of the GUI that won't be updated.
-- Returns a reference the todo list body and the footer with information.
--
-- Admittedly pretty hairy, but there's no real "logic" here, only view
-- manipulation.  If we had a high-level DOM manipulation library for ghcjs
-- this could probably just be half as long and much more clean.
renderInitial :: Document
              -> VNodePresentation
              -> Chan (Either TodoInp GUIInp)
              -> IO ()
renderInitial doc vbody inputChan = do
  Just hd <- documentGetHead doc

  meta <- createAppend doc hd "meta" castToHTMLMetaElement
  elementSetAttribute meta (jstr "charset") (jstr "utf-8")

  title <- createAppend doc hd "title" castToHTMLTitleElement
  htmlTitleElementSetText title (jstr "auto :: TodoMVC")

  forM_ ["assets/base.css","assets/index.css"] $ \lnk -> do
    cssLink <- createAppend doc hd "link" castToHTMLLinkElement
    htmlLinkElementSetRel cssLink (jstr "stylesheet")
    elementSetAttribute cssLink (jstr "charset") (jstr "utf-8")
    elementSetAttribute cssLink (jstr "type") (jstr "text/css")
    htmlLinkElementSetHref cssLink (jstr lnk)

  _ <- renderGui doc vbody inputChan (mempty, GUI All Nothing)
  return ()
  where
    jstr :: JSString -> JSString
    jstr = id

-- | Render the view for a given "output state" `(IntMap Task, GUIOpts)`,
-- and add the callbacks.
--
-- One thing to remember that is there is basically no logic going on here.
-- All we are doing is rendering the output of the `Auto` in a "dumb" way,
-- and adding callback hooks to add inputs into the `Chan` queue whenever
-- stuff happens.
--
-- If we had a nice high-level DOM language this could all pretty much be
-- half as long and very expressive...but yeah, the only "Auto" part is
-- that whenever someone clicks something or does something, it adds a new
-- command to the `Chan` queue.
--
-- Most of the fancy display tricks are handled with css, anyway :)
renderGui :: Document
          -> VNodePresentation
          -> Chan (Either TodoInp GUIInp)
          -> (IntMap Task, GUIOpts)
          -> IO Bool
renderGui doc vbody inputChan (tasks, gopts@(GUI filt selc)) = do
    renderVNode vbody inputChan tasks tasks' gopts allCompleted (IM.size uncompl) (IM.size compl)
    -- tells `runOnChan` that we want to continue.
    return True
  where
    tasks' = case filt of
              All       -> tasks
              Active    -> IM.filter (not . taskCompleted) tasks
              Completed -> IM.filter taskCompleted tasks
    allCompleted = all taskCompleted tasks
    (compl, uncompl) = IM.partition taskCompleted tasks

renderVNode :: VNodePresentation
            -> Chan (Either TodoInp GUIInp)
            -> IntMap Task
            -> IntMap Task
            -> GUIOpts
            -> Bool
            -> Int
            -> Int
            -> IO ()
renderVNode body inputChan tasks tasks' (GUI filt selc) allCompleted uncomplCount complCount = do
  renderTo body
    (into div_
     [with div_ (class_ ?= "todomvc-wrapper")
      [with section_ (id_ ?= "todoapp")
       [into header_
        [into h1_ ["todo"]
        , newToDo
        , main_
        , footer
        ]]
      , info
      ]])
  where
    main_ =
      with section_ (id_ ?= "main" >>
                     if IM.size tasks == 0
                     then hidden_ ?= "true"
                     else return ())
      [toggle_all, toggle_all_label, todo_list]
    footer =
      with footer_ (id_ ?= "footer" >>
                    if (IM.size tasks == 0)
                    then hidden_ ?= "true"
                    else return ())
      [todo_count, filters, clear_completed]
    newToDo =
      with input_ (
        onKeyPress [13]
        (\e _ -> do
            let inpe = castToHTMLInputElement e
            inp <- htmlInputElementGetValue inpe
            -- add an `IEAdd` command to the queue whenever a new task is submitted
            unless (null inp) $ do
              writeChan inputChan (Left (IEAdd inp))
              htmlInputElementSetValue inpe (""::String)) >>
        id_ ?= "new-todo" >>
        placeholder_ ?= "What needs to be done?" >>
        autofocus_ ?= "" >>
        name_ ?= "newTodo")
      []
    toggle_all =
      with input_ (
        -- send a new command to the queue whenever button is pressed
        on ("click") (\_ -> do
                         let newCompl = not allCompleted
                         writeChan inputChan (Left (IEAll (TEComplete newCompl)))) >>
        type_ ?= "checkbox" >>
        id_ ?= "toggle-all" >>
        name_ ?= "toggle" >>
        if allCompleted
        then checked_ ?= "true"
        else return ()) []
    toggle_all_label =
      with VD.label_ (for_ ?= "toggle-all") ["Mark all as complete"]
    todo_count =
      with VD.span_ (id_ ?= "todo-count")
      [into strong_ [text $ show uncomplCount]
      , " tasks left"]
    todo_list =
      with ul_ (id_ ?= "todo-list")
      $ IM.elems $ IM.mapWithKey renderTask tasks'

    filters =
      with ul_ (id_ ?= "filters") $
      map (\filtType ->
            with li_ (on ("click")
                      (\_ ->
                        -- send a new command to the queue whenever button is pressed
                        writeChan inputChan (Right (GIFilter filtType))) >>
                      href_ ?= "javascript:void();")
            [with a_ (if (filtType == filt)
                      then class_ ?= "selected"
                      else return ())
             [text (show filtType)]
            ])
      [All ..]
    clear_completed =
      with button_ (on ("click")
                    (\_ -> do
                      -- send a new command to the queue whenever button is pressed
                      writeChan inputChan (Left (IEAll TEPrune))) >>
                    id_ ?= "clear-completed" >>
                    class_ ?= "clear-completed" >>
                    type_ ?= "button" >>
                    if complCount == 0
                    then hidden_ ?= "true"
                    else return ()
                   ) [text $ "Clear completed (" <> show complCount <> ")"]
    info =
      with footer_ (id_ ?= "info")
      [into p_ ["Double-click to edit a todo"]
      , into p_
        ["Written by "
        , with a_ (href_ ?= "http://jle.im") ["Justin Le"]
        , " on "
        , with a_ (href_ ?= "https://github.com/ghcjs/ghcjs")
          [" ghcjs "]
        , "as an "
        , with a_ (href_ ?= "https://github.com/mstksg/auto")
          ["auto"]
        , " demo (source: "
        , with a_ (href_ ?= "https://github.com/mstksg/auto-examples/blob/master/src/Todo.hs")
          ["logic"]
        , with a_  (href_ ?= "https://github.com/mstksg/auto-examples/blob/master/src/TodoJS.hs") ["view)"]]
      , into p_ ["Spec, templates, and assets from "
                 , with a_ (href_ ?= "http://todomvc.com") ["TodoMVC"]]
      ]
    renderTask :: TaskID -> Task -> VDP.HTML
    renderTask tid t =
      with li_ (class_ ?= taskClass) [view, edit]
      where
        taskClass =
          toJSString . unwords . map snd . filter fst $ [ (taskCompleted t, "completed")
                                                        , (selc == Just tid, "editing")
                                                        ]
        view = with div_ (class_ ?= "view") [toggle, descr, destroy]
        toggle =
          with input_ (
            -- send a new command to the queue whenever button is pressed
            on "click"
            (\_ -> do
                let newCompl = not (taskCompleted t)
                writeChan inputChan (Left (IETask tid (TEComplete newCompl)))) >>
            type_ ?= "checkbox" >>
            class_ ?= "toggle" >>
            if taskCompleted t
            then checked_ ?= "true"
            else return ()) []
        descr =
          with VD.label_ (on "dblclick"
                          (\_ ->
                            writeChan inputChan (Right (GISelect (Just tid)))))
          [text (taskDescr t)]
        destroy =
          with button_ (on "click"
                        (\_ -> writeChan inputChan (Left (IETask tid TEDelete))) >>
                        class_ ?= "destroy")
          []
        edit =
          with input_ (class_ ?= "edit" >>
                       value_ ?= toJSString (taskDescr t) >>
                       name_ ?= "title" >>
                       id_ ?= toJSString ("todo-" <> show tid) >>
                       -- send a new command to the queue whenever button is pressed
                       onEl "blur" callback >>
                       onKeyPress [13, 27] (\e _ -> callback e))
          []
        callback :: HTMLElement -> IO ()
        callback e = do
          let inpe = castToHTMLInputElement e
          editString <- htmlInputElementGetValue inpe
          if null editString
            then writeChan inputChan (Left (IETask tid TEDelete))
            else do
            writeChan inputChan (Left (IETask tid (TEModify editString)))
            writeChan inputChan (Right (GISelect Nothing))

onEl :: MonadState VDP.HTMLElement m => JSString -> (HTMLElement -> IO ()) -> m ()
onEl nm cb = on (toJSString nm) f
  where
    f evt = do
      mjref <- fromJSRef evt
      case mjref of
       Just jref ->
         (eventTarget' evt) >>= maybe (return ()) cb
       Nothing ->
         return ()

onKeyPress :: MonadState VDP.HTMLElement m => [Int] -> (HTMLElement -> Int -> IO ()) -> m ()
onKeyPress keys cb = on "keypress" f
  where
    f evt = do
      mjref <- fromJSRef evt
      case mjref of
       Just jref -> do
         let uie = unsafeCastGObject (toGObject jref)::UIEvent
         key <- uiEventGetKeyCode uie
         if key `elem` keys
           then (eventTarget' evt) >>= maybe (return ()) (flip cb $ key)
           else return ()
       Nothing ->
         return ()

foreign import javascript safe
  "$1.target"
  ffiEventTarget :: JSRef Event -> JSRef HTMLElement

eventTarget' evt = do
  jref <- fromJSRef $ ffiEventTarget evt
  return $ (castToHTMLElement . toGObject) <$> jref

-- Utility function to create an item on the document with a given type and
-- tag and append it to a given parent.
createAppend :: ( IsDocument self
                , IsNode parent
                , IsNode b
                )
             => self
             -> parent
             -> JSString
             -> (Element -> b)
             -> IO b
createAppend doc parent tag coercer = do
    c@(Just child) <- fmap coercer <$> documentCreateElement doc tag
    _ <- nodeAppendChild parent c
    return child
