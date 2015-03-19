{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for experiments

module Util (dynMapAccum) where

import Control.Auto
import Control.Auto.Core
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Auto.Blip.Internal
import Data.IntMap               (IntMap, Key)
import Data.Traversable
import Prelude hiding            ((.), id, sequence)
import qualified Data.IntMap     as IM

dynMapAccum :: forall a b c d k s m. (Monad m, Applicative m)
            => (Key -> a -> s -> (b, s))
            -> (Key -> c -> s -> (d, s))
            -> s
            -> (k -> Interval m b c)
            -> a
            -> Auto m (IntMap a, Blip [k]) (IntMap d, s)
dynMapAccum g1 g2 s0 f x0 = go 0 IM.empty IM.empty
  where
    go i ks as = mkAutoM undefined
                         undefined
                       $ \(xs, news) -> do
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
