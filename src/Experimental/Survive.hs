{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Auto.Run
import Control.Lens
import Control.Monad                (unless)
import Control.Monad.Fix
import Data.Foldable
import Data.IntMap.Strict           (IntMap, (!))
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Serialize
import Debug.Trace
import GHC.Generics
import Linear
import Prelude hiding               ((.), id, elem, any)
import System.Console.ANSI
import System.IO
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M

data Dir = DUp | DRight | DDown | DLeft
         deriving (Show, Eq, Enum, Ord, Read, Generic)

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
             deriving (Show, Eq, Ord, Read, Generic)

data Entity = EPlayer | EBomb | EWall
            deriving (Show, Eq, Enum, Ord, Read, Generic)

instance Serialize EntResp
instance Serialize Dir
instance Serialize Entity

type Point        = V2 Int
type GameMap      = Map Point [Entity]
type EntityMap    = IntMap (Point, Entity)
type EntityInput  = (Point, EntityMap)
type EntityOutput = ((Point, Entity), Blip EntResp)

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

bomb :: Monad m => Dir -> Interval m EntityInput EntityOutput
bomb dir = proc _ -> do
    motion <- fromInterval zero . onFor 5 . pure (dirToV2 dir) -< ()
    onFor 10 . (id &&& never) -< (motion, EBomb)


wall :: Monad m => Interval m EntityInput EntityOutput
wall = toOn . (id &&& never) . pure (zero , EWall)

player :: Monad m => Interval m ((Cmd, Point), EntityMap) EntityOutput
player = proc ((inp, pos), world) -> do
    move <- fromBlipsWith zero dirToV2 . emitJusts (preview _CMove) -< inp
    atkB <- emitJusts (preview _CAtk)  -< inp
    toOn -< ((move, EPlayer), uncurry toResp <$> atkB)
  where
    toResp :: Usable -> Dir -> EntResp
    toResp u = case u of
                 Sword -> ERAtk
                 Bow   -> ERShoot
                 Bomb  -> ERBomb
                 Wall  -> ERBuild
    move x0 dx = clamp (x0 + dx)
    clamp = liftA3 (\mn mx -> max mn . min mx) (V2 0 0) mapSize

locomotor :: Monad m
          => V2 Int
          -> Interval m (a, EntityMap) EntityOutput
          -> Interval m (a, EntityMap) EntityOutput
locomotor p0 entA = proc inp@(_, world) -> do
    outp <- entA -< inp
    pos  <- fst <$> accum f (p0, False) -< (world, maybe zero (fst.fst) outp)
    id -< set (_1._1) pos <$> outp
  where
    f (p,mvd) (world, motion) = (restrict (p ^+^ motion), True)
      where
        world' = IM.mapMaybe getBlockers world
        restrict p' | not mvd          = p'
                    | p' `elem` world' = p
                    | otherwise        = p'

    getBlockers (pos, ent) | isBlocking ent = Just pos
                           | otherwise      = Nothing

    isBlocking ent = case ent of
                       EPlayer -> True
                       EWall   -> True
                       EBomb   -> False

game :: MonadFix m => Auto m Cmd GameMap
game = proc inp -> do
    rec pOut@((pPos,pEnt), respB) <- fromJust <$> locomotor startPos player -< first (inp,) (entInp ! (-1))

        let newEnts = (:[]) . (pPos,) <$> respB
        entInp <- arrD mkEntInp IM.empty -< fst <$> ents'

        ents <- dynMapF makeEntity (zero, IM.empty) -< (entInp, newEnts)

        let ents' = IM.insert (-1) pOut ents

    let entMap = M.fromListWith (<>)
               . IM.elems
               . fmap (second (:[]) . fst)
               $ ents

    id -< M.insertWith (<>) pPos [pEnt] entMap
  where
    mkEntInp :: IntMap (Point, Entity) -> IntMap EntityInput
    mkEntInp ents = IM.mapWithKey (\i (p,_) -> (p, IM.filterWithKey (\i' _ -> i' /= i) ents)) ents
    makeEntity :: Monad m => (Point, EntResp) -> Interval m EntityInput EntityOutput
    makeEntity (pPos, er) = case er of
                              ERBomb dir  -> locomotor pPos (bomb dir)
                              ERBuild dir -> locomotor (pPos + dirToV2 dir) wall
                              _           -> off

handleCmd :: (Serialize b, Monoid b, Monad m)
          => Auto m Cmd b
          -> Auto m (Maybe Cmd) b
handleCmd a0 = holdWith mempty . perBlip a0 . onJusts

renderBoard :: GameMap -> String
renderBoard mp = unlines . reverse
               $ [[ charAt x y | x <- [0..xMax] ] | y <- [0..yMax]]
  where
    charAt x y = fromMaybe '.' $ do
      es <- M.lookup (V2 x y) mp
      e  <- listToMaybe es
      M.lookup e entChrMap
    xMax = view _x mapSize
    yMax = view _y mapSize
    entChrMap = M.fromList [ (EPlayer, '@')
                           , (EBomb  , 'o')
                           , (EWall  , '#')
                           ]

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
    renderStdout (M.singleton startPos [EPlayer])
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
