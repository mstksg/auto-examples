{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Time
import Control.Auto.Run
import Control.Lens
import Control.Monad                (unless, guard)
import Control.Monad.Fix
import Data.Foldable
import Data.IntMap.Strict           (IntMap, Key)
import Data.List                    (sortBy)
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Ord
import Data.Serialize
import Data.Traversable             (sequence)
import Debug.Trace
import GHC.Generics
import Linear
import Prelude hiding               ((.), id, elem, any, sequence, concatMap, sum)
import System.Console.ANSI
import System.IO
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M

data Dir = DUp | DRight | DDown | DLeft
         deriving (Show, Eq, Enum, Ord, Read, Generic)

data Usable = Sword | Bow | Bomb | Wall
            deriving (Show, Eq, Enum, Ord, Read, Generic)

data Item = Potion
          deriving (Show, Eq, Enum, Ord, Read, Generic)

data Cmd = CMove Dir
         | CAct Usable Dir
         | CUse Item
         | CNop
         deriving (Show, Eq, Ord, Read, Generic)

data EntResp = ERAtk Double Point
             | ERShoot Dir
             | ERBomb Dir
             | ERBuild Dir
             | ERFire Double Int Point
             | ERPlayer Point
             deriving (Show, Eq, Ord, Read, Generic)

data EntComm = ECAtk Double
             deriving (Show, Eq, Ord, Read, Generic)

data Entity = EPlayer | EBomb | EWall | EFire
            deriving (Show, Eq, Enum, Ord, Read, Generic)

data EntityInput a = EI { _eiData  :: a         -- encodes input that only some can touch
                        , _eiPos   :: Point
                        , _eiComm  :: [(Key, EntComm)]
                        , _eiWorld :: EntityMap
                        } deriving (Show, Eq, Ord, Read, Functor)

data EntityOutput = EO { _eoPoint  :: Point
                       , _eoEntity :: Entity
                       , _eoResps  :: Maybe [EntResp]     -- f encodes alive/dead as Maybe, but only some can touch.
                       } deriving (Show, Eq, Ord, Read, Generic)


type Point         = V2 Int
type GameMap       = Map Point [Entity]
type EntityMap     = IntMap (Point, Entity)

instance Serialize EntResp
instance Serialize EntComm
instance Serialize Dir
instance Serialize Entity
instance Serialize Cmd
instance Serialize Item
instance Serialize Usable
instance Serialize EntityOutput

instance Serialize a => Serialize (EntityInput a) where
    put (EI x p c w) = put x *> put p *> put c *> put w
    get              = EI <$> get <*> get <*> get <*> get

instance Applicative EntityInput where
    pure x = EI x zero mempty mempty
    EI f p0 c0 w0 <*> EI x p1 c1 w1 = EI (f x) (p0 `v` p1) (c0 ++ c1) (w0 <> w1)
      where
        v y (V2 0 0) = y
        v _ y        = y

instance Semigroup a => Semigroup (EntityInput a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (EntityInput a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Semigroup Cmd where
    x <> CNop = x
    _ <> x = x

instance Monoid Cmd where
    mempty = CNop
    mappend x CNop = x
    mappend _    x = x


makePrisms ''Cmd
makePrisms ''EntResp
makePrisms ''EntComm
makeLenses ''EntityInput
makeLenses ''EntityOutput

mapSize :: V2 Int
mapSize = V2 70 20

startPos :: V2 Int
startPos = (`div` 2) <$> mapSize

dirToV2 :: Dir -> V2 Int
dirToV2 dir = case dir of
                DUp    -> V2 0    1
                DRight -> V2 1    0
                DDown  -> V2 0    (-1)
                DLeft  -> V2 (-1) 0

bomb :: Monad m => Dir -> Interval m (EntityInput a) EntityOutput
bomb dir = proc ei -> do
    motion <- fromInterval zero . onFor 8 . pure (dirToV2 dir) -< ()

    let damage = sumOf (eiComm . traverse . _2 . _ECAtk) ei

    trigger <- became (<= 0) . sumFrom 2 -< negate damage
    fuse    <- inB 10                    -< 0

    let explode = explodes <$ (fuse `mergeL` trigger)

    explosion <- fromBlips [] -< explode

    before -?> lmap fst (onFor 1) -< (EO motion EBomb (Just explosion), explode)
  where
    explodes = do
      x <- [-3..3]
      y <- [-3..3]
      let r = sqrt (fromIntegral x**2 + fromIntegral y**2) :: Double
      guard $ r <= 3
      let dur | r < 1     = 1
              | r < 2     = 1
              | otherwise = 1
          str | r < 1     = 6
              | r < 2     = 3
              | r < 3     = 2
              | otherwise = 1
      return $ ERFire str dur (V2 x y)

fire :: Monad m
     => Double
     -> Int
     -> Interval m (EntityInput a) EntityOutput
fire str dur = onFor dur . pure (EO zero EFire (Just [ERAtk str zero]))


wall :: Monad m => Auto m (EntityInput a) EntityOutput
wall = pure (EO zero EWall (Just []))

withHealth :: Monad m
           => Double
           -> Auto m (EntityInput a) EntityOutput
           -> Interval m (EntityInput a) EntityOutput
withHealth h0 entA = proc ei -> do
    eOut <- entA -< ei
    let damage = sumOf (eiComm . traverse . _2 . _ECAtk) ei
    die <- became (<= 0) . sumFrom 3 -< negate damage
    before -?> dead -< (eOut , die)
  where
    dead  = lmap (set eoResps Nothing . fst) (onFor 1)



player :: Monad m => Auto m (EntityInput Cmd) EntityOutput
player = proc (EI inp p _ world) -> do
    moveB            <- modifyBlips dirToV2
                      . emitJusts (preview _CMove)   -< inp

    (atkMvB, moveB') <- forkB isAtkMv                -< (,(p, world)) <$> moveB

    actB             <- modifyBlips toResp
                      . emitJusts (preview _CAct)    -< inp
    let allActB = actB `mergeL` (ERAtk 1.0 . fst <$> atkMvB)

    move <- fromBlips zero -< fst <$> moveB'
    allAct <- fromBlipsWith [] (:[]) -< allActB

    id -< EO move EPlayer (Just allAct)
  where
    isAtkMv :: (Point, (Point, EntityMap)) -> Bool
    isAtkMv (m,(p,em)) = any (\(p',e) -> p' == (p+m) && attackIt e) em
    toResp :: (Usable, Dir) -> EntResp
    toResp (u,d) = case u of
                     Sword -> ERAtk 1.0 (dirToV2 d)
                     Bow   -> ERShoot d
                     Bomb  -> ERBomb d
                     Wall  -> ERBuild d
    attackIt e = case e of
                   EPlayer -> False
                   EWall   -> True
                   EBomb   -> False
                   EFire   -> False

locomotor :: Monad m
          => Point
          -> Interval m (EntityInput a) EntityOutput
          -> Interval m (EntityInput a) EntityOutput
locomotor p0 entA = proc inp@(EI _ _ _ world) -> do
    outp <- entA -< inp
    pos  <- fst <$> accum f (p0, False) -< (world, maybe zero (view eoPoint) outp)
    id    -< set eoPoint pos <$> outp
  where
    f :: (Point, Bool) -> (EntityMap, Point) -> (Point, Bool)
    f (p, mvd) (world, motion) = (restrict (p ^+^ motion), True)
      where
        world' = IM.mapMaybe getBlockers world
        restrict p' | not mvd          = p'
                    | p' `elem` world' = clamp' p
                    | otherwise        = clamp' p'
        clamp' | clamp p == p = clamp
               | otherwise    = id
    clamp = liftA3 (\mn mx -> max mn . min mx) (V2 0 0) mapSize
    getBlockers (pos, ent) | isBlocking ent = Just pos
                           | otherwise      = Nothing
    isBlocking ent = case ent of
                       EPlayer -> True
                       EWall   -> True
                       EBomb   -> False
                       EFire   -> False

game :: MonadFix m => Auto m Cmd [GameMap]
game = accelerateWith CNop 2 $ proc inp -> do
    rec mkPlayer <- immediately -< [(zero, ERPlayer startPos)]

        entOuts <- dynMapF makeEntity (pure CNop) -< (allInp', newEntsB')

        let entOuts' = IM.filter (has (eoResps . _Just)) entOuts
            attacks  = collectAttacks entOuts'
            entInp   = mkEntInp entOuts'
            allInp   = IM.unionWith (<>) entInp attacks
            newEnts  = toList entOuts' >>= \(EO p _ ers) -> maybe [] (map (p,)) ers

        allInpD <- delay IM.empty -< allInp

        newEntsB <- lagBlips . emitOn (not . null) -< newEnts

        let allInp' = set (traverse . eiData) inp allInpD
            newEntsB' = merge (<>) mkPlayer newEntsB


    let entMap = M.fromListWith (<>)
               . IM.elems
               . fmap (\(EO p e _) -> (p, [e]))
               $ entOuts'

    id -< entMap
  where
    mkEntInp :: IntMap EntityOutput -> IntMap (EntityInput Cmd)
    mkEntInp eos = (`IM.mapWithKey` ents) $ \i (p,_) ->
                                     (EI CNop p [] (IM.delete i ents))
      where
        ents = fmap (\(EO p e _) -> (p, e)) eos
    makeEntity :: Monad m
               => (Point, EntResp)
               -> Interval m (EntityInput Cmd) EntityOutput
    makeEntity (p, er) = case er of
                           ERPlayer _   -> locomotor placed . withHealth 10 $ player
                           ERBomb dir   -> stretchy . locomotor placed $ bomb dir
                           ERBuild _    -> stretchy . locomotor placed . withHealth 3 $ wall
                           ERFire s d _ -> stretchy . locomotor placed $ fire s d
                           _            -> off
      where
        placed = place p er
        stretchy = stretchAccumBy (<>) (set (_Just . eoResps . _Just) []) 2


    place :: Point -> EntResp -> Point
    place p er = case er of
                   ERAtk _ disp -> p ^+^ disp
                   ERBomb  _    -> p
                   ERBuild dir  -> p ^+^ dirToV2 dir
                   ERShoot dir  -> p ^+^ dirToV2 dir
                   ERPlayer p'  -> p'
                   ERFire _ _ d -> p ^+^ d

    collectAttacks :: Monoid a => IntMap EntityOutput -> IntMap (EntityInput a)
    collectAttacks ents = fmap (\as -> set eiComm as mempty) $
      (`IM.mapWithKey` ents) $ \i (EO p _ _) ->
        let filtEnts = IM.delete i ents
            atks = (`IM.mapWithKey` filtEnts) $ \i' (EO p' _ ers) ->
                     flip mapMaybe (fromMaybe [] ers) $ \er -> do
                       ERAtk a _ <- Just er
                       guard $ place p' er == p
                       Just (ECAtk a)
        in  concatMap sequence $ IM.toList atks

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
      fmap entChr . listToMaybe . sortBy (comparing entPri) $ es
    xMax = view _x mapSize
    yMax = view _y mapSize
    entChr e = case e of
                 EPlayer -> '@'
                 EBomb   -> 'o'
                 EWall   -> '#'
                 EFire   -> '"'
    entPri e = case e of
                 EPlayer -> 0 :: Int
                 EBomb   -> 10
                 EWall   -> 2
                 EFire   -> 1

parseCmd :: Auto m Char (Blip (Maybe Cmd))
parseCmd = go Nothing
  where
    go Nothing  = mkAuto_ $ \x -> case x of
                    'h' -> (Blip (Just (CMove DLeft )) , go Nothing     )
                    'j' -> (Blip (Just (CMove DDown )) , go Nothing     )
                    'k' -> (Blip (Just (CMove DUp   )) , go Nothing     )
                    'l' -> (Blip (Just (CMove DRight)) , go Nothing     )
                    '5' -> (Blip (Just (CUse Potion )) , go Nothing     )
                    ' ' -> (Blip (Just CNop)           , go Nothing     )
                    '1' -> (NoBlip                     , go (Just Sword))
                    '2' -> (NoBlip                     , go (Just Bow  ))
                    '3' -> (NoBlip                     , go (Just Bomb ))
                    '4' -> (NoBlip                     , go (Just Wall ))
                    _   -> (Blip Nothing               , go Nothing     )
    go (Just u) = mkAuto_ $ \x -> case x of
                    'h' -> (Blip (Just (CAct u DLeft )), go Nothing     )
                    'j' -> (Blip (Just (CAct u DDown )), go Nothing     )
                    'k' -> (Blip (Just (CAct u DUp   )), go Nothing     )
                    'l' -> (Blip (Just (CAct u DRight)), go Nothing     )
                    _   -> (Blip Nothing               , go Nothing     )


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    renderStdout [ M.singleton startPos [EPlayer] ]
    _ <- runM generalize getChar process $ holdWith []
                                         . perBlip (handleCmd game)
                                         . parseCmd
    return ()
  where
    renderStdout mps = forM_ mps $ \mp -> do
      clearScreen
      putStrLn ""
      putStrLn (renderBoard mp)
    process mps = do
      unless (null mps) $ renderStdout mps
      Just <$> getChar


generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity
