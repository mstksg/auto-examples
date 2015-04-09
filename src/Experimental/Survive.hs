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
import Control.Lens
import Control.Monad.Fix
import Control.Monad.Random
import Control.Monad.Reader hiding  (forM_, mapM_)
import Control.Monad.Writer hiding  ((<>), forM_, mapM_)
import Data.Foldable
import Data.IntMap.Strict           (IntMap, Key)
import Data.List                    (sortBy)
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Ord
import Data.Serialize
import Debug.Trace
import GHC.Generics hiding          (to)
import Linear hiding                (ei, trace)
import Prelude hiding               ((.), id, elem, any, sequence, concatMap, sum, concat, sequence_, mapM_)
import System.Console.ANSI
import System.IO
import Util
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M


-- | Types for commands, entities, inputs, outputs, etc.

-- direction
data Dir = DUp | DRight | DDown | DLeft
         deriving (Show, Eq, Enum, Ord, Read, Generic)

-- an action to perform
data Action = Sword
            | Bow
            | Bomb
            | Wall
            deriving (Show, Eq, Enum, Ord, Read, Generic)

-- an item to use
data Item = Potion
          deriving (Show, Eq, Enum, Ord, Read, Generic)

-- something you can pick up
data Pickup = PUArrows
            | PUGunpowder
            | PUCement
            | PUPotion Double
            | PUGold Int
            deriving (Show, Eq, Ord, Read, Generic)

-- a command from the outside world/interface
data Cmd = CMove Dir
         | CAct Action Dir
         | CUse Item
         | CNop
         deriving (Show, Eq, Ord, Read, Generic)

-- a way an Entity can respond to the world
data EntResp = ERAtk Double Point       -- attack with damage at relative position
             | ERShoot Double Int Dir   -- shoot with damage and range in direction
             | ERBomb Dir               -- bomb in direction
             | ERBuild Dir              -- build in direction
             | ERFire Double Int Point  -- start a fire with damage and duration in relative position
             | ERMonster Char Double Double Point -- create a monster with sprite with health
                                                  --   and damage and absolute position
             | ERItem Pickup Point      -- place an item with pickup at absolute position
             | ERGive Key Pickup        -- give an entity with key/id a pickup
             deriving (Show, Eq, Ord, Read, Generic)

-- communications an Entity can receive, from another
data EntComm = ECAtk Double       -- attack with damage
             | ECGive Pickup      -- give pickup
             deriving (Show, Eq, Ord, Read, Generic)

-- an entity existing on the map
data Entity = EPlayer
            | EBomb
            | EWall
            | EFire
            | EMonster Char
            | EItem Pickup
            deriving (Show, Eq, Ord, Read, Generic)

-- input for an Entity auto
data EntityInput = EI { _eiPos   :: Point             -- new position
                      , _eiComm  :: [(Key, EntComm)]  -- communications, from id's
                      , _eiWorld :: EntityMap         -- a map of the world
                      } deriving (Show, Eq, Ord, Read, Generic)

-- output for an Entity auto
data EntityOutput a = EO { _eoData   :: Maybe a   -- extra data; Nothing if none.
                         , _eoPos    :: Point     -- position to move from
                         , _eoMove   :: Point     -- move
                         , _eoEntity :: Entity    -- the entity
                         , _eoReact  :: Map Entity Double -- "how this would react" when
                                                          --   encountering various entities;
                                                          --   how much damage it would attack with
                         , _eoResps  :: Maybe [EntResp]   -- lists of responses to the world.
                                                          --   Nothing if *dead*
                         } deriving (Show, Eq, Ord, Read, Generic)

-- output type from the player to the gui/frontend
data PlayerOut = PO { _poMessages  :: [OutMessage]    -- status messages
                    , _poHealth    :: Double          -- health
                    , _poInventory :: Inventory       -- inventory
                    , _poKills     :: Int             -- kill count
                    } deriving (Show, Eq, Ord, Read, Generic)

-- player inventory, for purpose of PlayerOut rendering. not actaully used
-- for the actual inventory updating of the player itself.
data Inventory = Inv { _invArrows    :: Int
                     , _invGunpowder :: Int
                     , _invCement    :: Int
                     , _invGold      :: Int
                     } deriving (Show, Eq, Ord, Read, Generic)

-- a status message to the outside world
data OutMessage = OMAtk Entity Entity Double  -- attack from to damage
                | OMShot Entity Entity Double -- shot from to damage
                | OMMiss Entity               -- shot missed by entity
                | OMDeath Entity              -- entity dies
                | OMPickup Entity Pickup      -- entity picked up picup
                deriving (Show, Eq, Ord, Read, Generic)

type Point         = V2 Int
type GameMap       = Map Point [Entity]
type EntityMap     = IntMap (Point, Entity)

instance Serialize EntResp
instance Serialize EntComm
instance Serialize Dir
instance Serialize Pickup
instance Serialize Entity
instance Serialize EntityInput
instance Serialize a => Serialize (EntityOutput a)
instance Serialize Cmd
instance Serialize Item
instance Serialize Action
instance Serialize Inventory
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
    PO m1 h1 i1 k1 <> PO m2 h2 i2 k2 = PO (m1 ++ m2) (h1 <#> h2) (i1 <> i2) (k1 <#> k2)
      where
        x <#> (-1) = x
        _ <#> y    = y

instance Monoid PlayerOut where
    mempty  = PO [] (-1) mempty (-1)
    mappend = (<>)

instance Semigroup Inventory where
    Inv a1 g1 c1 r1 <> Inv a2 g2 c2 r2 = Inv (a1 <#> a2) (g1 <#> g2) (c1 <#> c2) (r1 <#> r2)
      where
        x <#> (-1) = x
        _ <#> y    = y

instance Monoid Inventory where
    mempty = Inv (-1) (-1) (-1) (-1)
    mappend = (<>)

makePrisms ''Cmd
makePrisms ''Pickup
makePrisms ''Item
makePrisms ''EntResp
makePrisms ''EntComm
makePrisms ''Entity
makeLenses ''EntityInput
makeLenses ''EntityOutput
makeLenses ''PlayerOut
makeLenses ''Inventory

-- | Utility functions
mapSize :: V2 Int
mapSize = V2 70 20

startPos :: V2 Int
startPos = (`div` 2) <$> mapSize

initialPO :: PlayerOut
initialPO = PO [] initialHealth initialInv 0

initialInv :: Inventory
initialInv = Inv 50 10 30 0

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

-- | Entity `Auto`s
--
bomb :: Monad m
     => Dir
     -> Interval m EntityInput (EntityOutput a)
bomb dir = proc ei -> do
    -- move constantly
    motion <- fromInterval zero . onFor 8 . pure (dirToV2 dir) -< ()

    -- damage received
    let damage = sumOf (eiComm . traverse . _2 . _ECAtk) ei

    -- trigger: explosion from damage; fuse: explosion from timeout
    trigger <- became (<= 0) . sumFrom 2 -< negate damage
    fuse    <- inB 10                    -< 0

    -- explode when either `trigger` or `fuse` emit
    let explode = explodes <$ (fuse `mergeL` trigger)

    explosion <- fromBlips [] -< explode

    -- act like the EntityOutput until explosion; then just be on for 1.
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

-- immediately just attack everything and die.
fire :: Monad m
     => Double
     -> Int
     -> Interval m EntityInput (EntityOutput a)
fire str dur = lmap (\ei -> EO Nothing (_eiPos ei) zero EFire M.empty (Just [ERAtk str zero])) (onFor dur)

-- just sit there and do nothing.
wall :: Monad m
     => Auto m EntityInput (EntityOutput a)
wall = arr $ \ei -> EO Nothing (_eiPos ei) zero EWall M.empty (Just [])

-- sit there and do nothing, but when the player steps on you, send them an
-- `ERGive` response.
itemPu :: Monad m => Pickup -> Point -> Interval m EntityInput (EntityOutput (Double, a))
itemPu pu p0 = proc ei -> do
    pos <- onFor 1 . pure p0 <|!> id -< _eiPos ei      -- ignore first ei
    let pPos = preview (eiWorld . ix (-1) . _1) ei

    pickedB <- emitOn (uncurry (==)) -< (Just pos, pPos)
    picked <- fromBlips [] -< [ERGive (-1) pu] <$ pickedB

    let eOut = EO Nothing pos zero (EItem pu) M.empty (Just picked)

    naturalDeath <- inB 200 -< ()

    before -?> dead -< (eOut, (() <$ pickedB) <> naturalDeath)
  where
    dead = lmap fst (onFor 1) -?> lmap (set eoResps Nothing . fst) (onFor 1)

-- take an 'Auto' that never dies, and imbues it with health and death.
-- teaches an 'Auto' how to die.
withHealth :: MonadWriter ([OutMessage], Sum Int) m
           => Double
           -> Auto m EntityInput (EntityOutput (Double, a))
           -> Interval m EntityInput (EntityOutput (Double, a))
withHealth h0 entA = proc ei -> do
    eOut <- entA -< ei
    let damage = sumOf (eiComm . traverse . _2 . _ECAtk) ei

    health <- sumFrom h0 -< negate damage

    -- set the EntityOutput data field to be its health
    let eOut' = set (eoData . _Just . _1) (max 0 health) eOut

    die <- became (<= 0) -< health

    -- send a mesage if a monster dies
    if has (eoEntity . _EMonster) eOut
      then arrMB tell -< ([OMDeath (_eoEntity eOut)], 1) <$ die
      else never      -< ()

    -- send a message if the player dies
    if has (eoEntity . _EPlayer) eOut
      then arrMB (tell . (,mempty)) -< [OMDeath (_eoEntity eOut)] <$ die
      else never                    -< ()

    before -?> dead -< (eOut' , die)
  where
    dead  = lmap (set eoResps Nothing . fst) (onFor 1)

-- the player. move around, send out attacks, pick up recharges, drain
-- inventory....
player :: MonadReader Cmd m   -- environment is the current command
       => Auto m EntityInput (EntityOutput (Double, Inventory))
player = proc (EI p comm _) -> do
    inp <- effect ask -< ()
    move <- fromBlips zero
          . modifyBlips dirToV2
          . emitJusts (preview _CMove) -< inp

    resps <- fromBlipsWith [] (:[])
           . modifyBlips toResp
           . emitJusts (preview _CAct) -< inp

    arrowUsage     <- emitJusts $ preview (traverse . _ERShoot) -< resps
    gunpowderUsage <- emitJusts $ preview (traverse . _ERBomb) -< resps
    cementUsage    <- emitJusts $ preview (traverse . _ERBuild) -< resps

    getArrow     <- emitOn (> 0) -< length . toListOf (traverse . _2 . _ECGive . _PUArrows)    $ comm
    getGunpowder <- emitOn (> 0) -< length . toListOf (traverse . _2 . _ECGive . _PUGunpowder) $ comm
    getCement    <- emitOn (> 0) -< length . toListOf (traverse . _2 . _ECGive . _PUCement)    $ comm

    arrows    <- scanPos (_invArrows initialInv)    -< merge (+) ((-1) <$ arrowUsage)     (15 <$ getArrow)
    gunpowder <- scanPos (_invGunpowder initialInv) -< merge (+) ((-1) <$ gunpowderUsage) ( 5 <$ getGunpowder)
    cement    <- scanPos (_invCement initialInv)    -< merge (+) ((-1) <$ cementUsage)    (15 <$ getCement)

    gold         <- sumFrom 0 -< sumOf (traverse . _2 . _ECGive . _PUGold) comm

    let resps' = filter (enough arrows gunpowder cement) resps

    id -< EO (Just (initialHealth, Inv arrows gunpowder cement gold)) p move EPlayer atkMap (Just resps')
  where
    toResp :: (Action, Dir) -> EntResp
    toResp (u,d) = case u of
                     Sword -> ERAtk 4 (dirToV2 d)
                     Bow   -> ERShoot 1 15 d
                     Bomb  -> ERBomb d
                     Wall  -> ERBuild d
    atkMap = M.fromList . map (,4) $ [EWall, EMonster 'Z', EBomb]
    scanPos = scanB (\x y -> max 0 (x + y))
    enough ar gp cm resp = case resp of
                             ERAtk {}   -> True
                             ERShoot {} -> ar > 0
                             ERBomb {}  -> gp > 0
                             ERBuild {} -> cm > 0
                             _          -> True

-- move towards the player if it exists, or move around randomly if not.
monster :: MonadRandom m
        => Char
        -> Double
        -> Auto m EntityInput (EntityOutput a)
monster c damg = proc ei -> do
    let pPos  = ei ^? eiWorld . traverse . filtered (has (_2 . _EPlayer)) . _1
        mPos  = _eiPos ei
        delta = (^-^ mPos) <$> pPos
        moves = flip fmap delta $ \(V2 dx dy) ->
                  let adx = abs dx
                      ady = abs dy
                  in  (V2 (signum dx) 0 <$ guard (adx /= 0))
                  <|> (V2 0 (signum dy) <$ guard (ady /= 0))

    move <- during (arrM uniform) -< moves
    wander <- effect (dirToV2 `liftM` uniform [DUp ..]) -< ()

    let move' = fromMaybe wander move

    id -< EO Nothing mPos move' (EMonster c) atkMap (Just [])
  where
    atkMap = M.fromList . map (,damg) $ [EPlayer, EWall, EBomb]

-- the main game loop
game :: MonadFix m
     => StdGen
     -> Auto m Cmd (PlayerOut, GameMap)
game g = proc inp -> do
    -- run game', get the outputs, , count kills, save the last output,
    -- output to the client.
    (((eo, _), gm), (msgs, newKills)) <- game' -< inp
    kills <- sumFrom 0 -< getSum newKills
    lastEoDat <- holdJusts
            <|!> pure (initialHealth, initialInv) -< _eoData =<< eo
    let (hlth, inv) = lastEoDat
    let po = PO msgs hlth inv kills
    id -< (po, gm)
  where
    -- run the Writer and the Random over 'bracketA playerA worldA'
    -- "bracketA" runs player, then world, then player, so that the player
    -- gets a chance to "clean up".
    -- bracketA :: Auto m (Either a b) c -> Auto m c b -> Auto m a c runs
    -- the first on the `a` Right input, feeds the `c` into the second,
    -- runs the `b` output onto the first's Left channel, and outputs the
    -- final `c`.
    game' = runWriterA (sealRandomStd (bracketA playerA worldA) g)
    playerA :: (MonadFix m, MonadWriter ([OutMessage], Sum Int) m)
            => Auto m (Either Cmd EntityInput)
                      ( ( Maybe (EntityOutput (Double, Inventory))
                        , IntMap EntityInput
                        )
                      , GameMap
                      )
    -- manage the player input and wrapping the `player` Auto
    playerA = proc inp -> do
        -- last received world is the last world received from `Right`
        lastWorld <- holdWith IM.empty . emitJusts (preview (_Right . eiWorld)) -< inp
        rec lastPos <- delay startPos -< currPos
            -- new entity input for player
            let ei = set eiPos lastPos . either (const mempty) id $ inp
            -- run it through player', with the input
            pEo <- player' -< (ei, either id (const CNop) inp)
            -- generate the resulting entity inputs for everyone else, and
            -- messages
            let (pEis, msgs) = IM.mapAccumWithKey (mkEntIns lastWorld) IM.empty $ maybe IM.empty (IM.singleton (-1)) pEo

            -- keep the current position; move when the player intputs ask
            -- the player to move
            currPos <- holdWith startPos . emitJusts (preview (ix (-1) . eiPos)) -< pEis

        -- log the messages; messages are ([OutMessage], Sum Int) (kill count)
        arrM (tell . (,mempty)) -< toListOf (traverse . traverse) msgs

        let outEo = set (_Just . eoPos) currPos pEo
            outEi = IM.delete (-1) pEis
            outGm = either (const M.empty) (mkGMap lastPos . _eiWorld) inp

        id -< ((outEo, outEi), outGm)
      where
        -- imbue position, health, and take an extra parameter as the
        -- Reader environment
        player' = runReaderA . booster startPos . withHealth initialHealth $ player
        mkGMap p = M.fromListWith (<>)
                 . IM.elems
                 . (fmap . second) (:[])
                 . IM.insert (-1) (p, EPlayer)

    -- the rest of the world
    worldA :: (MonadFix m, MonadWriter ([OutMessage], Sum Int) m, MonadRandom m)
           => Auto m ( ( Maybe (EntityOutput (Double, a))
                       , IntMap EntityInput
                       ), GameMap
                     )
                     EntityInput
    worldA = proc ((pEo, pEis), _) -> do
        -- make things... monsters and items
        mkMonsters <- makeMonsters 25 -< ()
        mkItems    <- makeItems 15 -< ()

        -- run all of the entities on all of the inputs, using dynMapF
        rec entOuts <- dynMapF makeEntity mempty -< ( -- inputs from player and inputs from entities
                                                      IM.unionWith (<>) pEis entInsD'
                                                      -- make-new-entity events from everywhere
                                                    , newEntsBAll <> mkMonsters <> mkItems
                                                    )

                -- only alive
            let entOutsAlive = IM.filter (has (eoResps . _Just)) entOuts
                -- alive + player entity output
                entOutsFull  = maybe entOutsAlive (\po -> IM.insert (-1) po entOutsAlive) pEo
                -- map of all locations and entities
                entMap       = (_eoPos &&& _eoEntity) <$> entOutsFull
                -- generate new entity inputs from the entity outputs
                (entIns,msgs) = IM.mapAccumWithKey (mkEntIns entMap) IM.empty entOutsAlive
                -- update entity maps
                entMap'      = maybe id (\po -> IM.insert (-1) (_eoPos po, EPlayer)) pEo
                             . flip IM.mapMaybeWithKey entIns $ \k ei -> do
                                 eo <- IM.lookup k entOutsFull
                                 return (_eiPos ei, _eoEntity eo)
                entIns'      = flip IM.mapWithKey entIns $ \k -> set eiWorld (IM.delete k entMap')

                -- new entities, to send in as blip stream
                newEnts      = toList entOutsAlive >>= \(EO _ p _ _ _ ers) -> maybe [] (map (p,)) ers

                -- EntResps from player
                plrEResps    = toListOf (_Just . eoResps . _Just . traverse) pEo
                plrEResps'   = case pEo of
                                 Nothing -> []
                                 Just po -> (_eoPos po,) <$> plrEResps

            -- emit all non-empty newEnts, from "last cycle"
            newEntsB <- lagBlips . emitOn (not . null) -< newEnts
            -- all entity inputs from last cycle, to send into `entOuts`
            entInsD  <- delay IM.empty                 -< entIns'
            -- add in the player entity to the world maps
            let entInsD' = case pEo of
                             Just po -> over (traverse . eiWorld) (IM.insert (-1) (_eoPos po, EPlayer)) entInsD
                             Nothing -> entInsD

            playerB  <- emitOn (not . null) -< plrEResps'

            let newEntsBAll  = newEntsB <> playerB

        -- write messages to log; messages are ([OutMessage], Sum Int) (kill count)
        arrM (tell . (,mempty)) -< toListOf (traverse . traverse) msgs

        id -< set eiWorld (IM.delete (-1) entMap') . IM.findWithDefault mempty (-1) $ entIns'
      where
        makeMonsters :: MonadRandom m => Int -> Auto m a (Blip [(Point, EntResp)])
        makeMonsters n = onFor 500 . perBlip makeMonster . every n
                     --> makeMonsters ((n * 3) `div` 4)
        makeMonster :: MonadRandom m => Auto m a [(Point, EntResp)]
        makeMonster = liftA2 (\x y -> [(zero, ERMonster 'Z' 5 5 (shift (V2 x y)))])
                             (effect (getRandomR (0, view _x mapSize `div` 2)))
                             (effect (getRandomR (0, view _y mapSize `div` 2)))
          where
            shift = liftA2 (\m x -> (x - (m `div` 4)) `mod` m) mapSize

        makeItems :: MonadRandom m => Double -> Auto m a (Blip [(Point, EntResp)])
        makeItems r = perBlip makeItem . bernoulliMR (1/r)
          where
            makeItem = liftA3 (\x y i -> [(zero, ERItem i (shift (V2 x y)))])
                              (effect (getRandomR (0, 2 * view _x mapSize `div` 3)))
                              (effect (getRandomR (0, 2 * view _y mapSize `div` 3)))
                              (effect randomItem)
            shift = liftA2 (\m x -> (x + (m `div` 6))) mapSize
            randomItem = do
              x <- fromList [ (PUArrows, 1.5)
                            , (PUGunpowder, 1)
                            , (PUCement, 1)
                            , (PUPotion 0, 1)
                            , (PUGold 0, 1)
                            ]
              case x of
                PUGold _   -> PUGold `liftM` getRandomR (5,15)
                PUPotion _ -> PUPotion `liftM` getRandomR (10,40)
                _          -> return x

    -- start off at give position
    booster p0 a = (onFor 1 . arr (set (_Just . eoPos) p0) --> id) . a

    -- generating entity inputs from entity outputs of last round.  kinda
    -- complicated, but this is the beef of the game logic, having every
    -- entity communicate with every other one.  run using
    -- `IM.mapAccumWithKey`
    mkEntIns :: EntityMap             -- world map
             -> IntMap EntityInput    -- current "output" map, in-progress
             -> Key                   -- key of this processed entity
             -> EntityOutput a        -- entity output of this processed entity
             -> (IntMap EntityInput, [OutMessage])  -- updated "output" map, and also communications
    mkEntIns em eis k (EO _ pos0 mv e react (Just resps)) = (IM.insertWith (<>) k res withGives, messages)
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
                       ERShoot a rg d ->   -- TODO: drop when miss
                         let rg'     = fromIntegral rg
                             oldHits = flip IM.mapMaybe em' $ \(p, e') -> do
                                         guard $ arrowHit e'
                                         dst <- aligned pos2 p d
                                         dst <$ guard (dst <= rg')
                             newHits = flip IM.mapMaybeWithKey eis $ \k' ei -> do
                                         guard $ arrowHit (snd (em IM.! k'))
                                         dst <- aligned pos2 (_eiPos ei) d
                                         dst <$ guard (dst <= rg')
                             allHits = oldHits <> newHits
                             minHit  = fst . minimumBy (comparing snd) $ IM.toList allHits
                         in  Just $ if IM.null allHits
                                      then IM.singleton k (mempty, [OMMiss e])
                                      else IM.singleton minHit (set eiComm [(k, ECAtk a)] mempty, [OMShot e (snd (em IM.! minHit)) a])
                       _          ->
                         Nothing

        respGives = IM.unionsWith (<>) . flip mapMaybe resps $ \r ->
                      case r of
                        ERGive k' pu -> Just $ IM.singleton k' (set eiComm [(k, ECGive pu)] mempty, [OMPickup (snd (em IM.! k')) pu])
                        _            -> Nothing

        allAtks  = colAtks <> respAtks
        messages = toListOf (traverse . traverse)
                 $ IM.unionWith (<>) (snd <$> allAtks) (snd <$> respGives)

        withAtks = IM.unionWith (<>) (fst <$> IM.delete k allAtks) eis
        withGives = IM.unionWith (<>) (fst <$> respGives) withAtks
        res      = EI pos2 [] em'
        isBlocking ent = case ent of
                           EPlayer    -> True
                           EWall      -> True
                           EBomb      -> True
                           EFire      -> False
                           EMonster _ -> True
                           EItem _    -> False
        aligned :: Point -> Point -> Dir -> Maybe Double
        aligned p0 p1 dir = norm r <$ guard (abs (dotted - 1) < 0.001)
          where
            r      = fmap fromIntegral (p1 - p0) :: V2 Double
            rUnit  = normalize r
            dotted = rUnit `dot` fmap fromIntegral (dirToV2 dir)
        arrowHit :: Entity -> Bool
        arrowHit ent = case ent of
                         EPlayer    -> True
                         EWall      -> False
                         EBomb      -> True
                         EFire      -> False
                         EMonster _ -> True
                         EItem _    -> False
    mkEntIns _ eis _ _ = (eis, [])
    clamp = liftA3 (\mn mx -> max mn . min mx) (V2 0 0) mapSize

    -- make entity from EntResp
    makeEntity :: (MonadRandom m, MonadWriter ([OutMessage], Sum Int) m)
               => (Point, EntResp)
               -> Interval m EntityInput (EntityOutput (Double, a))
    makeEntity (p, er) = case er of
        ERBomb dir        -> booster placed $ bomb dir
        ERBuild {}        -> booster placed . withHealth 50 $ wall
        ERMonster c h d _ -> booster placed . withHealth h  $ monster c d
        ERFire s d _      -> booster placed $ fire s d
        ERItem pu pos     -> itemPu pu pos
        ERAtk {}          -> off
        ERShoot {}        -> off
        ERGive {}         -> off
      where
        placed = place p er

    -- where to place entity, given initial point and resp?
    place :: Point -> EntResp -> Point
    place p er = case er of
                   ERAtk _ disp       -> p ^+^ disp
                   ERBomb {}          -> p
                   ERBuild dir        -> p ^+^ dirToV2 dir
                   ERShoot _ _ dir    -> p ^+^ dirToV2 dir
                   ERFire _ _ d       -> p ^+^ d
                   ERMonster _ _ _ p' -> p'
                   ERItem _ p'        -> p'
                   ERGive {}          -> zero


-- handle command stream
handleCmd :: (Serialize b, Monoid b, Monad m)
          => Auto m Cmd b
          -> Auto m (Maybe Cmd) b
handleCmd a0 = holdWith mempty . perBlip a0 . onJusts

-- render the board
renderBoard :: (PlayerOut, GameMap) -> String
renderBoard (PO msgs ph (Inv ar gp cm gd) k, mp) =
    unlines . concat $ [ map renderOM msgs
                       , "You dead!" <$ guard (ph <= 0)
                       , ["[1] Sword\t[2] Bow (" ++ show ar ++ ")\t[3] Bomb (" ++ show gp ++ ")\t[4] Wall (" ++ show cm ++ ")"]
                       , mapOut
                       , ["Health: " ++ show (round ph :: Int) ++ "\tKills: " ++ show k ++ "\tGold: " ++ show gd]
                       ]
  where
    renderOM om = case om of
                    OMAtk e1 e2 d  -> [entChr e1] ++ " attacked " ++ [entChr e2] ++ " for " ++ show d ++ " HP"
                    OMShot e1 e2 d -> [entChr e1] ++ " shot " ++ [entChr e2] ++ " for " ++ show d ++ " HP"
                    OMMiss e1      -> "Shot from " ++ [entChr e1] ++ " missed!"
                    OMDeath e1     -> [entChr e1] ++ " died"
                    OMPickup e1 pu -> [entChr e1] ++ " picked up " ++ showPu pu
    mapOut = reverse [[ charAt x y | x <- [0..xMax] ] | y <- [0..yMax]]
    charAt x y = fromMaybe '.' $ do
      es <- M.lookup (V2 x y) mp
      let es' | ph > 0 = es
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
                 EItem pu   -> puChr pu
    entPri e = case e of
                 EPlayer    -> 0 :: Int
                 EFire      -> 1
                 EMonster _ -> 2
                 EBomb      -> 4
                 EItem _    -> 5
                 EWall      -> 6
    puChr pu = case pu of
                 PUArrows    -> '>'
                 PUGunpowder -> '%'
                 PUCement    -> '='
                 PUPotion _  -> '?'
                 PUGold _    -> '*'
    showPu pu = case pu of
                  PUArrows    -> "arrows"
                  PUGunpowder -> "gunpowder"
                  PUCement    -> "cement"
                  PUPotion _  -> "an unimplemented potion"
                  PUGold amt  -> show amt ++ " gold"


-- primitive command parser
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
    g <- newStdGen
    hSetBuffering stdin NoBuffering
    renderStdout (initialPO, M.singleton startPos [EPlayer])
    _ <- runM generalize getChar process $ hold
                                         . perBlip (handleCmd (game g))
                                         . parseCmd
    return ()
  where
    renderStdout mp = do
      clearScreen
      putStrLn ""
      putStrLn (renderBoard mp)
    process mp' = do
      mapM_ renderStdout mp'
      Just <$> getChar

-- turn Identity into IO
generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity
