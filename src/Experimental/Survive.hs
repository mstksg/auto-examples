{-# LANGUAGE Arrows #-}

module Main where

import Control.Auto
import Control.Auto.Run
import Control.Monad             (unless)
import Data.Map.Strict           (Map)
import Data.Maybe
import Data.Profunctor
import Linear
import Prelude hiding            ((.), id)
import System.Console.ANSI
import System.IO
import qualified Data.Map.Strict as M

data Dir = DUp | DRight | DDown | DLeft
         deriving (Show, Eq, Enum, Ord, Read)

player :: Monad m => Auto m Dir (V2 Int)
player = lmap dirToV2 (sumFrom (V2 0 0))
  where
    dirToV2 dir = case dir of
                    DUp    -> V2 0    1
                    DRight -> V2 1    0
                    DDown  -> V2 0    (-1)
                    DLeft  -> V2 (-1) 0

game :: Monad m => Auto m Dir (Map (V2 Int) Char)
game = proc inp -> do
    pPos <- player -< inp
    id -< M.singleton pPos '@'

renderBoard :: Map (V2 Int) Char -> String
renderBoard mp = unlines . reverse
               $ [[ charAt x y | x <- [-40..40] ] | y <- [-10..10]]
  where
    charAt x y = fromMaybe '.' $ M.lookup (V2 x y) mp

parseCmd :: Char -> Maybe Dir
parseCmd s = case s of
               'h' -> Just DLeft
               'j' -> Just DDown
               'k' -> Just DUp
               'l' -> Just DRight
               _   -> Nothing

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    renderStdout (M.singleton (V2 0 0) '@')
    _ <- runM generalize getChar process $ fromBlips M.empty
                                         . perBlip game
                                         . emitJusts parseCmd
    return ()
  where
    renderStdout mp = do
      clearScreen
      putStrLn ""
      putStrLn (renderBoard mp)
    process mp = do
      unless (M.null mp) $ renderStdout mp
      Just <$> getChar


generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity
