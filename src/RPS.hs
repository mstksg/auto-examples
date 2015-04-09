{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}

module RPS where

-- import Control.Auto.Core
-- import Control.Auto.Interval
-- import Control.Auto.Switch
-- import Data.Set                  (Set)
-- import qualified Data.Set        as S
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Monad.Fix
import Data.IntMap.Strict           (IntMap)
import Data.List
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Serialize
import GHC.Generics
import Prelude hiding               ((.), id)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M

data Throw = Rock | Paper | Scissors
           deriving (Show, Eq, Enum, Read, Ord, Generic)

data Message = MsgWon Bool Throw
             | MsgTie Throw
             deriving (Show, Generic)

data Output = Output { oP1Id    :: ID
                     , oP2Id    :: ID
                     , oP1Gone  :: Bool
                     , oP2Gone  :: Bool
                     , oScore1  :: Int
                     , oScore2  :: Int
                     , oTies    :: Int
                     , oMessage :: Maybe Message
                     } deriving (Show, Generic)

instance Serialize Throw
instance Serialize Output
instance Serialize Message

checkThrows :: Throw -> Throw -> Maybe Bool
checkThrows Rock     Paper    = Just False
checkThrows Rock     Scissors = Just True
checkThrows Paper    Rock     = Just True
checkThrows Paper    Scissors = Just False
checkThrows Scissors Rock     = Just False
checkThrows Scissors Paper    = Just True
checkThrows _        _        = Nothing

losesTo :: Throw -> Throw
losesTo Rock     = Scissors
losesTo Paper    = Rock
losesTo Scissors = Paper

beatenBy :: Throw -> Throw
beatenBy Rock     = Paper
beatenBy Paper    = Scissors
beatenBy Scissors = Rock

type ID = Int

collectGames :: MonadFix m => Auto m (ID, Maybe Throw) (IntMap Output)
collectGames = proc (k, mtr) -> do
    rec currGames <- arrD M.keys [] -< gameOuts

        let isInGame = find (\(x, y) -> k == x || k == y) currGames
        askNewB <- emitOn isNothing -< isInGame

        mkNewGame <- asMaybes . mapMaybeB id
                   . perBlip (mkState waiting Nothing) -< k <$ askNewB

        let gameInpK = isInGame <|> mkNewGame
            gameInp  = maybe M.empty (`M.singleton` (k, mtr)) gameInpK

        gameOuts <- gatherMany (uncurry game) -< gameInp

    id -< maybe IM.empty (getOuts gameOuts) gameInpK
  where
    getOuts :: Map (ID, ID) Output -> (ID, ID) -> IntMap Output
    getOuts mp k@(k1, k2) = IM.fromList $ case M.lookup k mp of
                              Just o  -> [(k1, o), (k2, invertOutput o)]
                              Nothing -> []
    waiting k st = case st of
                     Just k' | k /= k'   -> (Just (k', k), Nothing)
                             | otherwise -> (Nothing     , Just k')
                     Nothing -> (Nothing, Just k)
    invertOutput :: Output -> Output
    invertOutput (Output {..}) = Output oP2Id   oP1Id
                                        oP2Gone oP1Gone
                                        oScore2 oScore1 oTies
                                        (fmap invertMessage oMessage)
      where
        invertMessage (MsgWon p thr) = MsgWon (not p) thr
        invertMessage mt@(MsgTie _)  = mt


game :: Monad m => ID -> ID -> Interval m (ID, Maybe Throw) Output
game p1 p2 = proc (i, mti) -> do
    p1B <- emitJusts (getThrow p1) -< (i, mti)
    p2B <- emitJusts (getThrow p2) -< (i, mti)

    throwsB <- collectB -< (p1B, p2B)

    p1Gone <- holdWith False -< (False <$ throwsB) `mergeL` (True <$ p1B)
    p2Gone <- holdWith False -< (False <$ throwsB) `mergeL` (True <$ p2B)

    let resultB = score <$> throwsB

    p1Score <- scanB (+) 0 -< fst . fst <$> resultB
    p2Score <- scanB (+) 0 -< snd . fst <$> resultB
    ties    <- scanB (+) 0 -<       snd <$> resultB

    let messageScore = messageFrom <$> throwsB

    message <- asMaybes -< messageScore

    id -< Just $ Output p1 p2 p1Gone p2Gone p1Score p2Score ties message
  where
    getThrow p (i, Just x) | i == p = Just x
    getThrow _ _                    = Nothing
    score (t1, t2) = case checkThrows t1 t2 of
                       Just True  -> ((1, 0), 0)
                       Just False -> ((0, 1), 0)
                       Nothing    -> ((0, 0), 1)
    messageFrom (t1, t2) = case checkThrows t1 t2 of
                             Just True  -> MsgWon True t1
                             Just False -> MsgWon False t2
                             Nothing    -> MsgTie t1


