module Main (main) where

import Data.Map.Strict (Map)

type RoomID = Int

data Room = Room { roomID    :: RoomID
                 , roomName  :: String
                 , roomDesc  :: String
                 , roomItems :: [Item]
                 , roomLinks :: Map String RoomID
                 }

data Item = Item

main :: IO ()
main = putStrLn "adventure time!"


