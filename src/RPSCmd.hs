{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Text.Read
import Control.Applicative
import Control.Auto.Run
import Control.Concurrent
import Control.Monad                (forever, void)
import Data.Foldable
import Data.IntMap.Strict           (IntMap, Key)
import Data.Maybe
import Data.Serialize
import Data.Traversable
import Network.Simple.TCP
import RPS
import System.Environment
import qualified Data.IntMap.Strict as IM

-- main :: IO ()
-- main = print Scissors

-- type GameAuto m = Auto m (ID, Maybe Throw) (Map ID Output)

main :: IO ()
main = withSocketsDo $ do
    toServe   <- listToMaybe <$> getArgs
    case toServe of
      Just "client" -> client
      _             -> server

server :: IO ()
server = do
    socketMap <- newMVar (IM.empty, 0)
    inpChan   <- newChan :: IO (Chan (ID, Maybe Throw))

    void . forkIO . void $
      runOnChan (autoHandler socketMap) inpChan collectGames
    serve "127.0.0.1" "4050" (onSocket socketMap inpChan)
  where
    autoHandler :: MVar (IntMap Socket, Key) -> IntMap Output -> IO Bool
    autoHandler socketMap outputs = do
      _ <- flip IM.traverseWithKey outputs $ \k o -> do
        sock <- IM.lookup k . fst <$> readMVar socketMap
        forM_ sock $ \s -> send s (encode o)
      return True
    onSocket :: MVar (IntMap Socket, Key)
             -> Chan (ID, Maybe Throw)
             -> (Socket, SockAddr)
             -> IO ()
    onSocket socketMap inpChan (sock, addr) = do
        key <- modifyMVarMasked socketMap (return . addToMap)
        putStrLn $ "Connection on " ++ show addr ++ " (id = " ++ show key ++ ")"
        forever $ do
            inp <- fmap decode <$> recv sock 1024
            forM_ inp $ \inp' ->
              forM_ inp' $ \inp'' -> do
                putStrLn $ "[" ++ show key ++ "] " ++ show inp''
                writeChan inpChan (key, inp'')
      where
        addToMap (sm, k) = ((sm', k'), k')
          where
            k' = k + 1
            sm' = IM.insert k' sock sm

client :: IO ()
client = connect "127.0.0.1" "4050" $ \(sock, _) -> do
    putStrLn "Connected to server!"
    send sock (encode (Nothing :: Maybe Throw))
    putStrLn "Waiting for game..."
    Right resp <- decode <$> untilJust (recv sock 1024) :: IO (Either String Output)
    putStrLn $ "Game started against " ++ show (oP2Id resp) ++ "!"
    forever $ inputLoop sock
  where
    inputLoop :: Socket -> IO ()
    inputLoop sock = do
      putStrLn "Enter throw: (R/P/S)"
      thr <- untilJust (parseThrow <$> getLine)
      putStrLn $ "Sending " ++ show thr
      send sock (encode (Just thr))
      putStrLn "Waiting for opponent..."
      untilJust $ waitResp sock
    waitResp :: Socket -> IO (Maybe ())
    waitResp sock = do
      Right resp <- decode <$> untilJust (recv sock 1024) :: IO (Either String Output)
      forM (oMessage resp) $ \rmsg -> do
        putStrLn $ case rmsg of
          MsgWon True t  -> "Opponent threw " ++ show (losesTo t) ++ " and lost!"
          MsgWon False t -> "Opponent threw " ++ show t ++ " and won!"
          MsgTie t       -> "Opponent threw " ++ show t ++ " to tie."
        putStrLn $ "Score: "
                ++ show (oScore1 resp)
                ++ " - "
                ++ show (oScore2 resp)
                ++ " - "
                ++ show (oTies resp)

    parseThrow :: String -> Maybe Throw
    parseThrow thr = case words thr of
        "R":_ -> Just Rock
        "P":_ -> Just Paper
        "S":_ -> Just Scissors
        _     -> Nothing
    untilJust :: IO (Maybe a) -> IO a
    untilJust ima = maybe (untilJust ima) return =<< ima
