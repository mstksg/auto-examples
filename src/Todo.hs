{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Interval
import Control.Auto.Process
import Control.Auto.Run
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Data.Map                (Map)
import Data.Maybe
import Data.Monoid
import Data.Serialize
import GHC.Generics
import Prelude hiding          ((.), id, interact)
import Text.Read
import qualified Data.Map      as M

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

data InpEvent = IEAdd String
              | IETask Int TaskEvent
              deriving Show

data TaskEvent = TEDelete
               | TEComplete Bool
               | TEModify String
               deriving Show

data Task = Task { taskDescr     :: Maybe String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

instance Serialize Task

taskMap :: Monad m => Auto m (Int, TaskEvent) (Map Int Task)
taskMap = gather (const mkTask)
  where
    mkTask = mkAccum f (Just (Task Nothing False))
    f _        TEDelete       = Nothing
    f (Just t) (TEComplete c) = Just (t { taskCompleted = c        })
    f (Just t) (TEModify str) = Just (t { taskDescr     = Just str })
    f t        _              = t

parseInp :: String -> Maybe InpEvent
parseInp = p . words
  where
    p ("A":xs)   = Just (IEAdd (unwords xs))
    p ("D":n:_)  = readMaybe n <&> \n' -> IETask n' TEDelete
    p ("C":n:_)  = readMaybe n <&> \n' -> IETask n' (TEComplete True)
    p ("U":n:_)  = readMaybe n <&> \n' -> IETask n' (TEComplete False)
    p ("M":n:xs) = readMaybe n <&> \n' -> IETask n' (TEModify (unwords xs))
    p _          = Nothing

taskInp :: MonadFix m => Auto m String (Map Int Task)
taskInp = proc inpStr -> do
    inpEvtB <- emitJusts parseInp -< inpStr

    rec lastTM <- lastVal mempty -< taskMap

        let ids    = M.keys lastTM

        newTaskB <- perBlip newTask . mapMaybeB isAdd -< inpEvtB
        modTaskB <- mapMaybeB validTE                 -< (ids,) <$> inpEvtB

        let tmInpB = newTaskB `mergeL` modTaskB

        taskMap <- hold . perBlip taskMap <|!> pure mempty -< tmInpB

    id -< taskMap
  where
    validTE (ids, IETask n te) | n `elem` ids = Just (n, te)
    validTE _                                 = Nothing

    newTask = proc descr -> do
      newId <- count -< ()
      id -< (newId, TEModify descr)


isAdd :: InpEvent -> Maybe String
isAdd (IEAdd descr) = Just descr
isAdd _             = Nothing

formatTodo :: Map Int Task -> String
formatTodo = unlines . map format . M.toList
  where
    format (n, Task desc compl) = concat [ show n
                                         , ". ["
                                         , if compl then "X" else " "
                                         , "] "
                                         , fromMaybe "" desc
                                         ]

main :: IO ()
main = void . interact $ fmap (Just . formatTodo) taskInp
