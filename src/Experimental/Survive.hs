{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Control.Monad.Fix
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Auto.Process
import Control.Auto.Interval
import Control.Auto.Run
import Control.Lens
import Control.Monad              (unless)
import Data.Map.Strict            (Map)
import Data.Maybe
import Data.Serialize
import Linear
import Prelude hiding             ((.), id)
import System.Console.ANSI
import System.IO
import qualified Data.Map.Strict  as M

data Dir = DUp | DRight | DDown | DLeft
         deriving (Show, Eq, Enum, Ord, Read)

data Usable = Sword | Bow | Bomb | Wall
            deriving (Show, Eq, Enum, Ord, Read)

data Item = Potion
          deriving (Show, Eq, Enum, Ord, Read)

data Cmd = CMove Dir
         | CAtk Usable Dir
         | CUse Item
         deriving (Show, Eq, Ord, Read)

makePrisms ''Cmd

data EntResp = ERAtk Dir
             | ERShoot Dir
             | ERBomb Dir
             | ERBuild Dir
             deriving (Show, Eq, Ord, Read)


mapSize :: V2 Int
mapSize = V2 80 20

startPos :: V2 Int
startPos = (`div` 2) <$> mapSize

dirToV2 :: Dir -> V2 Int
dirToV2 dir = case dir of
                DUp    -> V2 0    1
                DRight -> V2 1    0
                DDown  -> V2 0    (-1)
                DLeft  -> V2 (-1) 0

bomb :: Monad m => V2 Int -> Dir -> Interval m a (V2 Int, Char)
bomb p0 dir = onFor 4 . arr (,'o') . sumFrom p0 . pure (dirToV2 dir)

player :: Monad m => Auto m Cmd (V2 Int, Blip EntResp)
player = proc inp -> do
    movesB <- emitJusts (preview _CMove) -< inp
    pos <- scanB move startPos -< dirToV2 <$> movesB

    atkB   <- emitJusts (preview _CAtk)  -< inp

    id -< (pos, uncurry toResp <$> atkB)
  where
    toResp :: Usable -> Dir -> EntResp
    toResp u = case u of
                 Sword -> ERAtk
                 Bow   -> ERShoot
                 Bomb  -> ERBomb
                 Wall  -> ERBuild
    move x0 dx = clamp (x0 + dx)
    clamp = liftA3 (\mn mx -> max mn . min mx) (V2 0 0) mapSize

game :: Monad m => Auto m Cmd (Map (V2 Int) Char)
game = proc inp -> do
    (pPos, respB) <- player            -< inp

    -- ents <- muxMany makeEntity -< (pPos,) <$> respB

    ents <- scanB addEntity M.empty . mapMaybeB toEntity -< (pPos,) <$> respB

    id -< M.insert pPos '@' ents
  where
    makeEntity = undefined
    addEntity mp e = uncurry M.insert e mp
    toEntity (pPos, er) = case er of
                            ERBuild dir -> Just (pPos + dirToV2 dir, '#')
                            ERBomb dir  -> Just (pPos + dirToV2 dir, 'o')
                            _           -> Nothing

handleCmd :: (Serialize b, Monoid b, Monad m) => Auto m Cmd b -> Auto m (Maybe Cmd) b
handleCmd a0 = holdWith mempty . perBlip a0 . onJusts

renderBoard :: Map (V2 Int) Char -> String
renderBoard mp = unlines . reverse
               $ [[ charAt x y | x <- [0..xMax] ] | y <- [0..yMax]]
  where
    charAt x y = fromMaybe '.' $ M.lookup (V2 x y) mp
    xMax = view _x mapSize
    yMax = view _y mapSize

parseCmd :: Auto m Char (Blip (Maybe Cmd))
parseCmd = go Nothing
  where
    go Nothing  = mkAuto_ $ \x -> case x of
                    'h' -> (Blip (Just (CMove DLeft )) , go Nothing     )
                    'j' -> (Blip (Just (CMove DDown )) , go Nothing     )
                    'k' -> (Blip (Just (CMove DUp   )) , go Nothing     )
                    'l' -> (Blip (Just (CMove DRight)) , go Nothing     )
                    '5' -> (Blip (Just (CUse Potion )) , go Nothing     )
                    '1' -> (NoBlip                     , go (Just Sword))
                    '2' -> (NoBlip                     , go (Just Bow  ))
                    '3' -> (NoBlip                     , go (Just Bomb ))
                    '4' -> (NoBlip                     , go (Just Wall ))
                    _   -> (Blip Nothing               , go Nothing     )
    go (Just u) = mkAuto_ $ \x -> case x of
                    'h' -> (Blip (Just (CAtk u DLeft )), go Nothing     )
                    'j' -> (Blip (Just (CAtk u DDown )), go Nothing     )
                    'k' -> (Blip (Just (CAtk u DUp   )), go Nothing     )
                    'l' -> (Blip (Just (CAtk u DRight)), go Nothing     )
                    _   -> (Blip Nothing               , go Nothing     )


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    renderStdout (M.singleton startPos '@')
    _ <- runM generalize getChar process $ holdWith M.empty
                                         . perBlip (handleCmd game)
                                         . parseCmd
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
