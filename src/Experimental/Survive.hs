{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Auto.Effects
import Control.Auto.Interval
import Control.Auto.Process.Random
import Control.Auto.Run
import Control.Auto.Time
import Control.Lens
import Control.Monad                (unless, guard, mfilter)
import Control.Monad.Fix
import Control.Monad.Reader hiding  (forM_)
import Control.Monad.Writer hiding  ((<>), forM_)
import Data.Foldable
import Data.IntMap.Strict           (IntMap, Key)
import Data.List                    (sortBy)
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Ord
import Data.Serialize
import Data.Traversable             (sequence)
import Debug.Trace
import GHC.Generics hiding          (to)
import Linear hiding                (ei, trace)
import Prelude hiding               ((.), id, elem, any, sequence, concatMap, sum, concat, sequence_)
import System.Console.ANSI
import System.IO
import System.Random
import Util
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
             | ERShoot Double Int Dir
             | ERBomb Dir
             | ERBuild Dir
             | ERFire Double Int Point
             | ERPlayer Point
             | ERMonster Char Double Double Point
             deriving (Show, Eq, Ord, Read, Generic)

data EntComm = ECAtk Double
             deriving (Show, Eq, Ord, Read, Generic)

data Entity = EPlayer | EBomb | EWall | EFire | EMonster Char
            deriving (Show, Eq, Ord, Read, Generic)

data EntityInput = EI { _eiPos   :: Point
                      , _eiComm  :: [(Key, EntComm)]
                      , _eiWorld :: EntityMap
                      } deriving (Show, Eq, Ord, Read, Generic)

data EntityOutput a = EO { _eoData   :: Maybe a
                         , _eoPos    :: Point  -- position to move from
                         , _eoMove   :: Point  -- move
                         , _eoEntity :: Entity
                         , _eoReact  :: Map Entity Double
                         , _eoResps  :: Maybe [EntResp]
                         } deriving (Show, Eq, Ord, Read, Generic)

data PlayerOut = PO { _poMessages :: [OutMessage]
                    , _poHealth   :: Double
                    } deriving (Show, Eq, Ord, Read, Generic)


data OutMessage = OMAtk Entity Entity Double
                | OMShot Entity Entity Double
                | OMMiss Entity
                deriving (Show, Eq, Ord, Read, Generic)

type Point         = V2 Int
type GameMap       = Map Point [Entity]
type EntityMap     = IntMap (Point, Entity)

instance Serialize EntResp
instance Serialize EntComm
instance Serialize Dir
instance Serialize Entity
instance Serialize EntityInput
instance Serialize a => Serialize (EntityOutput a)
instance Serialize Cmd
instance Serialize Item
instance Serialize Usable
instance Serialize PlayerOut
instance Serialize OutMessage

instance Semigroup EntityInput where
    EI p0 c0 w0 <> EI p1 c1 w1 = EI (p0 `v` p1) (c0 ++ c1) (w0 <> w1) -- watch out, is (<>) right here?
      where
        v y (V2 (-1) (-1)) = y      -- yeah this might not work
        v _ y              = y

instance Monoid EntityInput where
    mempty  = EI (V2 (-1) (-1)) mempty mempty
    mappend = (<>)

instance Semigroup Cmd where
    x <> CNop = x
    _ <> x = x

instance Monoid Cmd where
    mempty = CNop
    mappend x CNop = x
    mappend _    x = x

instance Semigroup PlayerOut where
    PO m1 h1 <> PO m2 h2 = PO (m1 ++ m2) (h1 <#> h2)
      where
        x <#> (-1) = x
        _ <#> y    = y

instance Monoid PlayerOut where
    mempty  = PO [] (-1)
    mappend = (<>)


makePrisms ''Cmd
makePrisms ''EntResp
makePrisms ''EntComm
makePrisms ''Entity
makeLenses ''EntityInput
makeLenses ''EntityOutput
makeLenses ''PlayerOut

mapSize :: V2 Int
mapSize = V2 70 20

startPos :: V2 Int
startPos = (`div` 2) <$> mapSize

initialPO :: PlayerOut
initialPO = PO [] initialHealth

initialHealth :: Double
initialHealth = 50

dirToV2 :: Dir -> V2 Int
dirToV2 dir = case dir of
                DUp    -> V2 0    1
                DRight -> V2 1    0
                DDown  -> V2 0    (-1)
                DLeft  -> V2 (-1) 0

v2ToDir :: V2 Int -> Maybe Dir
v2ToDir v2 = case v2 of
               V2 0    1    -> Just DUp
               V2 1    0    -> Just DRight
               V2 0    (-1) -> Just DDown
               V2 (-1) 0    -> Just DLeft
               _            -> Nothing


bomb :: Monad m
     => Dir
     -> Interval m EntityInput (EntityOutput a)
bomb dir = proc ei -> do
    motion <- fromInterval zero . onFor 8 . pure (dirToV2 dir) -< ()

    let damage = sumOf (eiComm . traverse . _2 . _ECAtk) ei

    trigger <- became (<= 0) . sumFrom 2 -< negate damage
    fuse    <- inB 10                    -< 0

    let explode = explodes <$ (fuse `mergeL` trigger)

    explosion <- fromBlips [] -< explode

    before -?> lmap fst (onFor 1) -< (EO Nothing (_eiPos ei) motion EBomb M.empty (Just explosion), explode)
  where
    explodes = do
      x <- [-3..3]
      y <- [-3..3]
      let r = sqrt (fromIntegral x**2 + fromIntegral y**2) :: Double
      guard $ r <= 3
      let dur | r < 1     = 2
              | r < 2     = 1
              | otherwise = 1
          str | r < 1     = 16
              | r < 2     = 8
              | r < 3     = 2
              | otherwise = 1
      return $ ERFire str dur (V2 x y)

fire :: Monad m
     => Double
     -> Int
     -> Interval m EntityInput (EntityOutput a)
fire str dur = lmap (\ei -> EO Nothing (_eiPos ei) zero EFire M.empty (Just [ERAtk str zero])) (onFor dur)


wall :: Monad m
     => Auto m EntityInput (EntityOutput a)
wall = arr $ \ei -> EO Nothing (_eiPos ei) zero EWall M.empty (Just [])

withHealth :: Monad m
           => Double
           -> Auto m EntityInput (EntityOutput Double)
           -> Interval m EntityInput (EntityOutput Double)
withHealth h0 entA = proc ei -> do
    eOut <- entA -< ei
    let damage = sumOf (eiComm . traverse . _2 . _ECAtk) ei

    health <- sumFrom h0 -< negate damage

    let eOut' = set (eoData . _Just) (max 0 health) eOut

    die <- became (<= 0) -< health
    before -?> dead -< (eOut' , die)
  where
    dead  = lmap (set eoResps Nothing . fst) (onFor 1)

player :: MonadReader Cmd m => Auto m EntityInput (EntityOutput Double)
player = proc (EI p _ world) -> do
    inp <- effect ask -< ()
    move <- fromBlips zero
          . modifyBlips dirToV2
          . emitJusts (preview _CMove) -< inp

    resps <- fromBlipsWith [] (:[])
           . modifyBlips toResp
           . emitJusts (preview _CAct) -< inp

    id -< EO (Just initialHealth) p move EPlayer atkMap (Just resps)
  where
    toResp :: (Usable, Dir) -> EntResp
    toResp (u,d) = case u of
                     Sword -> ERAtk 4 (dirToV2 d)
                     Bow   -> ERShoot 1 20 d
                     Bomb  -> ERBomb d
                     Wall  -> ERBuild d
    atkMap = M.fromList . map (,4) $ [EWall, EMonster 'Z', EBomb]

actionMap :: Map (Usable, Dir) EntResp
actionMap = M.fromList $ fmap (\d -> ((Sword, d), ERAtk 400 (dirToV2 d))) [DUp ..]
                      ++ fmap (\d -> ((Bow, d), ERShoot 400 20 d)) [DUp ..]
                      ++ fmap (\d -> ((Bomb, d), ERBomb d)) [DUp ..]
                      ++ fmap (\d -> ((Wall, d), ERBuild d)) [DUp ..]

monster :: Monad m
        => Char
        -> Double
        -> Auto m EntityInput (EntityOutput a)
monster c damg = proc ei -> do
    let pPos  = ei ^? eiWorld . traverse . filtered (has (_2 . _EPlayer)) . _1
        mPos  = _eiPos ei
        world = _eiWorld ei
        delta = (^-^ mPos) <$> pPos
        move  = flip (maybe zero) delta $ \(V2 dx dy) ->
                  let adx = abs dx
                      ady = abs dy
                  in  case () of
                        () | adx == 0  -> V2 0 (signum dy)
                           | ady == 0  -> V2 (signum dx) 0
                           | adx < ady -> V2 (signum dx) 0
                           | otherwise -> V2 0 (signum dy)

    id -< EO Nothing mPos move (EMonster c) atkMap (Just [])
  where
    atkMap = M.fromList . map (,damg) $ [EPlayer, EWall, EBomb]

game :: MonadFix m
     => StdGen
     -> Auto m Cmd [(Maybe PlayerOut, GameMap)]
game g = proc inp -> do
    (((eo, _), gm), msgs) <- runWriterA (bracketA playerA worldA) -< inp
    id -< [(fmap (PO msgs) . _eoData =<< eo, gm)]
  where
    playerA :: (MonadFix m, MonadWriter [OutMessage] m)
            => Auto m (Either Cmd EntityInput)
                      ( ( Maybe (EntityOutput Double)
                        , IntMap EntityInput
                        )
                      , GameMap
                      )
    playerA = proc inp -> do
        lastWorld <- holdWith IM.empty . emitJusts (preview (_Right . eiWorld)) -< inp
        rec lastPos <- delay startPos -< currPos
            let ei = set eiPos lastPos . either (const mempty) id $ inp
            pEo <- player' -< (ei, either id (const CNop) inp)
            let (pEis, msgs) = IM.mapAccumWithKey (mkEntIns lastWorld) IM.empty $ maybe IM.empty (IM.singleton (-1)) pEo
            currPos <- holdWith startPos . emitJusts (preview (ix (-1) . eiPos)) -< pEis

        arrM tell -< toListOf (traverse . traverse) msgs

        let outEo = set (_Just . eoPos) currPos pEo
            outEi = IM.delete (-1) pEis
            outGm = either (const M.empty) (mkGMap lastPos . _eiWorld) inp

        id -< ((outEo, outEi), outGm)
      where
        player' = runReaderA . booster startPos . withHealth initialHealth $ player
        mkGMap p = M.fromListWith (<>)
                 . IM.elems
                 . (fmap . second) (:[])
                 . IM.insert (-1) (p, EPlayer)

    worldA :: (MonadFix m, MonadWriter [OutMessage] m)
           => Auto m ( ( Maybe (EntityOutput Double)
                       , IntMap EntityInput
                       ), GameMap
                     )
                     EntityInput
    worldA = proc ((pEo, pEis), _) -> do
        mkMonsters <- makeMonsters 25 -< ()

        rec entOuts <- dynMapF makeEntity mempty -< (IM.unionWith (<>) pEis entInsD', newEntsBAll <> mkMonsters)         -- 1

            let entOutsAlive = IM.filter (has (eoResps . _Just)) entOuts              -- 1
                entOutsFull  = maybe entOutsAlive (\po -> IM.insert (-1) po entOutsAlive) pEo

                entMap       = (_eoPos &&& _eoEntity) <$> entOutsFull
                -- entIns - no trace of player
                (entIns,msgs) = IM.mapAccumWithKey (mkEntIns entMap) IM.empty entOutsAlive -- 1
                entMap'      = maybe id (\po -> IM.insert (-1) (_eoPos po, EPlayer)) pEo
                             . flip IM.mapMaybeWithKey entIns $ \k ei -> do           -- 1
                                 eo <- IM.lookup k entOutsFull
                                 return (_eiPos ei, _eoEntity eo)
                entIns'      = flip IM.mapWithKey entIns $ \k -> set eiWorld (IM.delete k entMap')      -- 1

                newEnts      = toList entOutsAlive >>= \(EO _ p _ _ _ ers) -> maybe [] (map (p,)) ers   -- 1

                plrEResps    = toListOf (_Just . eoResps . _Just . traverse) pEo      -- f
                plrEResps'   = case pEo of                                            -- f
                                 Nothing -> []
                                 Just po -> (_eoPos po,) <$> plrEResps

            newEntsB <- lagBlips . emitOn (not . null) -< newEnts                     -- 0
            entInsD  <- delay IM.empty                 -< entIns'                     -- 0
            let entInsD' = case pEo of
                             Just po -> over (traverse . eiWorld) (IM.insert (-1) (_eoPos po, EPlayer)) entInsD
                             Nothing -> entInsD

            playerB  <- emitOn (not . null) -< plrEResps'                             -- 1

            let newEntsBAll  = newEntsB <> playerB                                    -- 1

        arrM tell -< toListOf (traverse . traverse) msgs

        id -< set eiWorld (IM.delete (-1) entMap') . IM.findWithDefault mempty (-1) $ entIns'
      where
        makeMonsters :: Monad m => Int -> Auto m a (Blip [(Point, EntResp)])
        makeMonsters n = onFor 100 . perBlip makeMonster . every n
                     --> makeMonsters (n `div` 2)
        makeMonster :: Monad m => Auto m a [(Point, EntResp)]
        makeMonster = liftA2 (\x y -> [(zero, ERMonster 'Z' 5 5 (shift (V2 x y)))])
                             (stdRands (randomR (0, view _x mapSize `div` 2)) g)
                             (stdRands (randomR (0, view _y mapSize `div` 2)) g)
          where
            shift = liftA2 (\m x -> (x - (m `div` 4)) `mod` m) mapSize

    booster p0 a = (onFor 1 . arr (set (_Just . eoPos) p0) --> id) . a

    mkEntIns :: EntityMap
             -> IntMap EntityInput
             -> Key
             -> EntityOutput a
             -> (IntMap EntityInput, [OutMessage])
    mkEntIns em eis k (EO _ pos0 mv e react (Just resps)) = (IM.insertWith (<>) k res withAtks, messages)
      where
        em'      = IM.delete k em
        pos1     = pos0 ^+^ mv
        oldCols  = IM.mapMaybe (\(p,e') -> e' <$ guard (p == pos1)) em'
        newCols  = flip IM.mapMaybeWithKey eis $ \k' ei -> do
                     guard (_eiPos ei == pos1)
                     snd <$> IM.lookup k' em'
        allCols  = oldCols <> newCols
        pos2     | any isBlocking allCols = pos0
                 | otherwise              = clamp pos1    -- could be short circuited here, really...
        colAtks  = flip IM.mapMaybe allCols $ \e' -> do
                     d <- M.lookup e' react
                     return (over eiComm ((k, ECAtk d):) mempty, [OMAtk e e' d])
        respAtks = IM.unionsWith (<>) . flip mapMaybe resps $ \r ->
                     case r of
                       ERAtk a _ ->
                         let placed   = place pos2 r
                             oldHits  = snd <$> IM.filter (\(p,_) -> placed == p) em'
                             newHits  = flip IM.mapMaybeWithKey eis $ \k' ei -> do
                                          guard (placed == _eiPos ei)
                                          snd <$> IM.lookup k' em
                             allHits  = oldHits <> newHits
                         in  Just $ (\e' -> (set eiComm [(k, ECAtk a)] mempty, [OMAtk e e' a])) <$> allHits
                       ERShoot a rg d ->   -- todo: drop stuff when too close...alert hits?
                         let rg'     = fromIntegral rg
                             oldHits = flip IM.mapMaybe em' $ \(p, _) -> mfilter (<= rg') (aligned pos2 p d)
                             newHits = flip IM.mapMaybe eis $ \ei -> mfilter (<= rg') (aligned pos2 (_eiPos ei) d)
                             allHits = oldHits <> newHits
                             minHit  = fst . minimumBy (comparing snd) $ IM.toList allHits
                         in  Just $ if IM.null allHits
                                      then IM.singleton k (mempty, [OMMiss e])
                                      else IM.singleton minHit (set eiComm [(k, ECAtk a)] mempty, [OMShot e (snd (em IM.! minHit)) a])
                       _          ->
                         Nothing

        allAtks  = colAtks <> IM.delete k respAtks
        messages = toListOf (traverse . _2 . traverse) allAtks

        withAtks = IM.unionWith (<>) (fst <$> allAtks) eis
        res      = EI pos2 [] em'
        isBlocking ent = case ent of
                           EPlayer    -> True
                           EWall      -> True
                           EBomb      -> True
                           EFire      -> False
                           EMonster _ -> True
        aligned :: Point -> Point -> Dir -> Maybe Double
        aligned p0 p1 dir = norm r <$ guard (abs (dotted - 1) < 0.001)
          where
            r      = fmap fromIntegral (p1 - p0) :: V2 Double
            rUnit  = normalize r
            dotted = rUnit `dot` fmap fromIntegral (dirToV2 dir)
    mkEntIns _ eis _ _ = (eis, [])
    clamp = liftA3 (\mn mx -> max mn . min mx) (V2 0 0) mapSize
    makeEntity :: Monad m
               => (Point, EntResp)
               -> Interval m EntityInput (EntityOutput Double)
    makeEntity (p, er) = case er of
        -- ERPlayer _        -> booster placed . withHealth pHealth $ player
        ERBomb dir        -> stretchy . booster placed $ bomb dir
        ERBuild _         -> stretchy . booster placed . withHealth 25 $ wall
        ERMonster c h d _ -> stretchy . booster placed . withHealth h  $ monster c d
        ERFire s d _      -> stretchy . booster placed $ fire s d
        _                 -> off
      where
        placed = place p er
        -- stretchy = stretchAccumBy (<>) (set (_Just . eoResps . _Just) []) 2
        stretchy = id
    place :: Point -> EntResp -> Point
    place p er = case er of
                   ERAtk _ disp       -> p ^+^ disp
                   ERBomb  _          -> p
                   ERBuild dir        -> p ^+^ dirToV2 dir
                   ERShoot _ _ dir    -> p ^+^ dirToV2 dir
                   ERPlayer p'        -> p'
                   ERFire _ _ d       -> p ^+^ d
                   ERMonster _ _ _ p' -> p'

handleCmd :: (Serialize b, Monoid b, Monad m)
          => Auto m Cmd b
          -> Auto m (Maybe Cmd) b
handleCmd a0 = holdWith mempty . perBlip a0 . onJusts

renderBoard :: (Maybe PlayerOut, GameMap) -> String
renderBoard (po, mp) = case po of
                         Just (PO msgs ph) -> unlines . concat $ [ map renderOM msgs
                                                                 , mapOut
                                                                 , ["Health: " ++ show (round ph :: Int)]
                                                                 ]
                         Nothing           -> unlines [ "You dead!"
                                                      , unlines mapOut
                                                      , "Health: 0"
                                                      ]
  where
    renderOM om = case om of
                    OMAtk e1 e2 d -> [entChr e1] ++ " attacked " ++ [entChr e2] ++ " for " ++ show d ++ " HP"
                    OMShot e1 e2 d -> [entChr e1] ++ " shot " ++ [entChr e2] ++ " for " ++ show d ++ " HP"
                    OMMiss e1      -> "Shot from " ++ [entChr e1] ++ " missed!"
    mapOut = reverse [[ charAt x y | x <- [0..xMax] ] | y <- [0..yMax]]
    charAt x y = fromMaybe '.' $ do
      es <- M.lookup (V2 x y) mp
      let es' | isJust po = es
              | otherwise = filter (/= EPlayer) es
      fmap entChr . listToMaybe . sortBy (comparing entPri) $ es'
    xMax = view _x mapSize
    yMax = view _y mapSize
    entChr e = case e of
                 EPlayer    -> '@'
                 EBomb      -> 'o'
                 EWall      -> '#'
                 EFire      -> '"'
                 EMonster c -> c
    entPri e = case e of
                 EPlayer    -> 0 :: Int
                 EBomb      -> 10
                 EWall      -> 3
                 EFire      -> 1
                 EMonster _ -> 2

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
    -- print $ autoConstr (player :: Auto' (EntityInput Cmd) (EntityOutput PlayerOut))
    g <- newStdGen
    hSetBuffering stdin NoBuffering
    renderStdout [ (Just initialPO, M.singleton startPos [EPlayer]) ]
    _ <- runM generalize getChar process $ holdWith []
                                         . perBlip (handleCmd (game g))
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
