{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Auto hiding       (loop)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Generate
import Control.Auto.Run
import Control.Auto.Switch
import Control.Auto.Time
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.List hiding          (concat, all, any, and)
import Data.Map.Strict           (Map)
import Data.Serialize
import Data.Serialize
import GHC.Generics
import Prelude hiding            ((.), id, concat, all, any, and)
import System.Console.ANSI
import qualified Data.Map.Strict as M

type Board = [[Piece]]
type Player = Piece

data Piece = X | O deriving (Show, Read, Eq, Generic)

data BoardOut = BoardOut { boBoard  :: Board
                         , boWinner :: Maybe (Maybe Player)
                         , boNext   :: Player
                         } deriving Generic

instance Serialize Piece
instance Serialize BoardOut

boardWidth, boardHeight :: Int
boardWidth = 7
boardHeight = 6

emptyBoard :: Board
emptyBoard = replicate boardWidth []

isWinner :: Player -> Board -> Bool
isWinner p b = any (any hasFour) [ filled , transpose filled
                                 , wedgeUp, wedgeDown
                                 ]
  where
    hasFour (j:k:l:m:ns) | and [j,k,l,m] = True
                         | otherwise     = hasFour (k:l:m:ns)
    hasFour _                            = False
    filled    = map (take boardHeight . (++ repeat False) . map (== p)) b
    wedge     = take boardWidth . inits $ repeat False
    wedgeUp   = transpose $ zipWith (++) wedge filled
    wedgeDown = transpose $ zipWith (++) (reverse wedge) filled

showBoard :: Board -> String
showBoard = unlines   . map concat
          . transpose . map fill
  where
    fill :: [Piece] -> [String]
    fill = map (++ "|") . reverse . take boardHeight . (++ repeat "_") . map show

showOut :: BoardOut -> String
showOut (BoardOut brd winner nextP) =
    unlines [ unwords (map show [1..boardWidth])
            , showBoard brd
            , case winner of
                Nothing -> "To play: " ++ show nextP
                Just w  -> "Game over! " ++ case w of
                                              Just p  -> "Winner: " ++ show p
                                              Nothing -> "Tie game."
            ]

main :: IO ()
main = loop (duringRead (board emptyBoard X))
  where
    loop a = do
      inp <- getLine
      Output b a' <- stepAuto a inp
      case b of
        Nothing      -> return ()
        Just (bo, _) -> do
          clearScreen
          putStrLn (showOut bo)
          loop a'
-- main = void $ interactId (fmap (showOut . fst) <$> duringRead (board emptyBoard X))

board :: MonadFix m => Board -> Player -> Auto m Int (BoardOut, Bool)
board b0 p0 = switchFromF gameOver (board' b0 p0)
  where
    gameOver b = (pure (b, False) &&& id) . never

board' :: MonadFix m => Board -> Player -> Auto m Int ((BoardOut, Bool), Blip BoardOut)
board' b0 p0 = proc i -> do
    rec currP   <- mkAccum swapPlayer p0 . delay False -< goodMove
        brd     <- toList . fill <$> gather col -< (i, currP)
        lastBrd <- delay b0 -< brd
        let goodMove = lastBrd /= brd

    let winner | isWinner currP brd       = Just (Just currP)
               | length (concat brd) >= d = Just Nothing
               | otherwise                = Nothing
    win <- onJusts -< winner
    let boardOut = BoardOut brd winner (opp currP)
    id -< ((boardOut, goodMove), boardOut <$ win)
  where
    inRange n = n > 0 && n <= length b0
    d         = boardHeight * boardWidth
    fill      = M.unionWith (<>) (M.fromList (zip [1..] b0))
    col n | inRange n = Just <$> column (b0 !! (n - 1))
          | otherwise = pure Nothing
    opp X = O
    opp O = X
    swapPlayer p s | s         = opp p
                   | otherwise = p

column :: Monad m => [Piece] -> Auto m Piece [Piece]
column = mkAccum (\ps p -> take boardHeight (ps ++ [p]))



