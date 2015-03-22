{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utility functions for experiments

module Util (
    dynMapAccumF
  , mutual
  , bracketA
  , InteractiveF(..)
  , Interactive
  , interact
  , runInteractive
  , UnFreeT(..)
  , unfree
  , refree
  ) where

import Control.Auto
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Monad.Fix
import Control.Monad.Free.TH
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Data.Foldable
import Data.IntMap                (IntMap, Key)
import Data.Serialize             (Serialize)
import Data.Traversable
import Prelude hiding             ((.), id, sequence, mapM, mapM_, interact)
import qualified Data.IntMap      as IM
import qualified Data.Serialize   as S

bracketA :: Monad m
         => Auto m (Either a b) c
         -> Auto m c b
         -> Auto m a c
bracketA a1 a2 = mkAutoM undefined
                         undefined
                       $ \x -> do
                           (y , a1' ) <- stepAuto a1 (Left x)
                           (z , a2' ) <- stepAuto a2 y
                           (y', a1'') <- stepAuto a1' (Right z)
                           return (y', bracketA a1'' a2')

mutual :: (Monad m, Serialize d)
       => Auto m (a, d) c
       -> Auto m (b, c) d
       -> d
       -> Auto m (a, b) (c, d)
mutual a1 a2 z2 = mkAutoM (mutual <$> resumeAuto a1 <*> resumeAuto a2 <*> S.get)
                          (saveAuto a1 *> saveAuto a2 *> S.put z2)
                        $ \(x1, x2) -> do
                            (y1, a1') <- stepAuto a1 (x1, z2)
                            (y2, a2') <- stepAuto a2 (x2, y1)
                            return ((y1, y2), mutual a1' a2' y2)

dynMapAccumF :: forall a b c d k s m. (Monad m, Applicative m, Serialize k)
             => (Key -> a -> s -> (b, s))
             -> (Key -> c -> s -> (d, s))
             -> (k -> Interval m b c)
             -> a
             -> Auto m ((s, IntMap a), Blip [k]) (IntMap d, s)
dynMapAccumF g1 g2 f x0 = go 0 IM.empty IM.empty
  where
    go i ks as = mkAutoM (do i'  <- S.get
                             ks' <- S.get
                             as' <- mapM (resumeAuto . f) ks'
                             return (go i' ks' as') )
                         (S.put i *> S.put ks *> mapM_ saveAuto as)
                       $ \((s0, xs), news) -> do
                           let newks  = zip [i..] (blip [] id news)
                               newas  = (map . second) f newks
                               newks' = ks `IM.union` IM.fromList newks
                               newas' = as `IM.union` IM.fromList newas
                               newc   = i + length newks
                               resMap = zipIntMapWithDefaults (,) Nothing (Just x0) newas' xs
                           (res, s1) <- runStateT (IM.traverseWithKey t resMap) s0
                           let ys' = IM.mapMaybe fst res
                               as' = snd <$> IM.intersection res ys'
                               ks' = IM.intersection newks' ys'
                           return ((ys', s1), go newc ks' as')
    t :: Key -> (Interval m b c, a) -> StateT s m (Maybe d, Interval m b c)
    t k (a0, x0) = do
      x1 <- state (g1 k x0)
      (y0, a1) <- lift $ stepAuto a0 x1
      y1 <- case y0 of
              Just y0' -> Just <$> state (g2 k y0')
              Nothing  -> return Nothing
      return (y1, a1)

type MapMerge m k a b c = (k -> a -> b -> Maybe c)
                       -> (m a -> m c)
                       -> (m b -> m c)
                       -> m a -> m b -> m c

genericZipMapWithDefaults :: (Monoid (m c), Functor m)
                          => MapMerge m k a b c
                          -> (a -> b -> c) -> Maybe a -> Maybe b
                          -> m a -> m b -> m c
genericZipMapWithDefaults mm f x0 y0 = mm f' zx zy
  where
    f' _ x y = Just (x `f` y)
    zx = case y0 of
           Nothing -> const mempty
           Just y' -> fmap (`f` y')
    zy = case x0 of
           Nothing -> const mempty
           Just x' -> fmap (x' `f`)

zipIntMapWithDefaults :: (a -> b -> c) -> Maybe a -> Maybe b -> IntMap a -> IntMap b -> IntMap c
zipIntMapWithDefaults = genericZipMapWithDefaults IM.mergeWithKey


data InteractiveF p r a = Interact p (r -> a) deriving Functor

makeFree ''InteractiveF

runInteractive :: Monad m
               => (p -> m r)
               -> FreeT (InteractiveF p r) m a
               -> m a
runInteractive prompt iact = do
    ran <- runFreeT iact
    case ran of
      Pure x -> return x
      Free (Interact p f) -> do
        resp <- prompt p
        runInteractive prompt (f resp)

data UnFreeT t m a = UnFree { runUnFreeT :: (forall x. t m x -> m x) -> t m a }

unfree :: t m a -> UnFreeT t m a
unfree tm = UnFree $ \_ -> tm

instance MonadTrans t => MonadTrans (UnFreeT t) where
    lift = unfree . lift

refree :: (forall x. t m x -> m x) -> UnFreeT t m a -> t m a
refree interp x = runUnFreeT x interp

instance (MonadTrans t, Monad m, Monad (t m)) => Monad (UnFreeT t m) where
    return = lift . return
    x >>= k = UnFree $ \interp -> runUnFreeT x interp >>= refree interp . k

instance (MonadTrans t, MonadFix m, Monad (t m)) => MonadFix (UnFreeT t m) where
    mfix f = UnFree $ \interp -> lift . mfix $ interp . refree interp . f


type Interactive p r = UnFreeT (FreeT (InteractiveF p r))

