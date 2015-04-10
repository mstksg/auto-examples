{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
    inpChan   <- newChan :: IO (Chan (ID, Input))

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
             -> Chan (ID, Input)
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
    waitGame sock
  where
    waitGame :: Socket -> IO ()
    waitGame sock = do
      send sock (encode IJoin)
      putStrLn "Waiting for game..."
      Right resp <- decode <$> untilJust (recv sock 1024) :: IO (Either String Output)
      putStrLn $ "Game started against " ++ show (oP2Id resp) ++ "!"
      inputLoop sock
    inputLoop :: Socket -> IO ()
    inputLoop sock = do
      putStrLn "Enter throw: (R/P/S)"
      cmd <- untilJust (parseCmd <$> getLine)
      putStrLn $ "Sending " ++ show cmd
      send sock (encode cmd)
      putStrLn "Waiting for opponent..."
      (out, msg) <- untilJust $ waitResp sock
      case msg of
        MsgQuit True -> do
          putStrLn "Quit game!  Goodbye!"
          putStrLn $ "Final " ++ showScore out
        MsgQuit False -> do
          putStrLn "Opponent has quit!"
          putStrLn $ "Final " ++ showScore out
          putStrLn "Finding new game..."
          waitGame sock
        MsgRes w t   -> do
          putStrLn $ case w of
                       Just True  -> "Opponent threw " ++ show (losesTo t) ++ " and lost! :D"
                       Just False -> "Opponent threw " ++ show t ++ " and won! :("
                       Nothing    -> "Opponent threw " ++ show t ++ " to tie. :|"
          putStrLn $ showScore out
          inputLoop sock

    waitResp :: Socket -> IO (Maybe (Output, Message))
    waitResp sock = do
      Right resp <- decode <$> untilJust (recv sock 1024) :: IO (Either String Output)
      return ((resp,) <$> oMessage resp)

    parseCmd :: String -> Maybe Input
    parseCmd cmd = case words cmd of
        "R":_ -> Just $ IThrow Rock
        "P":_ -> Just $ IThrow Paper
        "S":_ -> Just $ IThrow Scissors
        "Q":_ -> Just IQuit
        _     -> Nothing
    showScore :: Output -> String
    showScore out = "Score: " ++ show (oScore1 out)
                 ++ " - "     ++ show (oScore2 out)
                 ++ " - "     ++ show (oTies   out)

    untilJust :: IO (Maybe a) -> IO a
    untilJust ima = maybe (untilJust ima) return =<< ima
