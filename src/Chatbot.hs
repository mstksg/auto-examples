{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main where

-- import Control.Auto.Serialize
import Control.Auto
import Control.Concurrent
import Data.Serialize
import Control.Exception
import Control.Monad
-- import Control.Monad
import Data.Map.Strict                 (Map)
import Data.Time
import Network.SimpleIRC
import Prelude hiding                  ((.), id)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict       as M

botName :: String
botName = "autobot-test"

channels :: [String]
channels = ["#autobot-test"]

chatbotFP :: FilePath
chatbotFP = "data/save/chatbot"

type ChatBot m = Auto m InMessage OutMessages
type ChatBot' m = Auto m InMessage [String]

data InMessage = InMessage { inMessageNick   :: String
                           , inMessageBody   :: String
                           , inMessageSource :: String
                           , inMessageTime   :: UTCTime
                           } deriving Show


newtype OutMessages = OutMessages { outMessageMap :: Map String [String]
                                  }

instance Monoid OutMessages where
    mempty = OutMessages mempty
    mappend (OutMessages a) (OutMessages b) = OutMessages (M.unionWith (<>) a b)

main :: IO ()
main = launchIRC chatBot $ \_ ->
         print "callback"

ircConf :: MVar (ChatBot IO) -> IrcConfig
ircConf a = (mkDefaultConfig "irc.freenode.org" botName)
              { cChannels = channels
              , cEvents   = [Privmsg (onMessage a)]
              }

launchIRC :: ChatBot IO -> (ChatBot IO -> IO ()) -> IO ()
launchIRC a0 cont = do
    amvr <- newMVar a0
    let launch = connect (ircConf amvr) False True
    void launch `catch` \(_ :: AsyncException) -> withMVar amvr cont

onMessage :: MVar (ChatBot IO) -> EventFunc
onMessage amvr server msg = do
    OutMessages resps <- modifyMVar amvr $ \a ->
      case (mNick msg, mOrigin msg) of
        (Just nick, Just orig) -> do
          t <- getCurrentTime
          let inmsg = InMessage (C8.unpack nick) (C8.unpack (mMsg msg))
                                (C8.unpack orig) t
          Output out a' <- stepAuto a inmsg
          return (a', out)
        _   ->
          return (a, mempty)
    _ <- flip M.traverseWithKey resps $ \k v ->
        mapM_ (sendMsg server (C8.pack k) . C8.pack) v
    return ()

chatBot :: Monad m => ChatBot m
chatBot = mconcat [perRoom seenBot]

perRoom :: Monad m => ChatBot' m -> ChatBot m
perRoom cb' = proc im -> do
    outs <- cb' -< im
    id -< OutMessages $ M.singleton (inMessageSource im) outs

seenBot :: Monad m => ChatBot' m
seenBot = proc (InMessage nick msg _ time) -> do
    seens <- mkAccum (\m (n, t) -> M.insert n t m) mempty -< (nick, time)

    id -< case words msg of
            "@seen":req:_ ->
              [ case M.lookup req seens of
                  Just t  -> "'" ++ req ++ "' last seen at " ++ show t ++ "."
                  Nothing -> "No record of '" ++ req ++ "'." ]
            _             ->
              mzero


instance Serialize UTCTime where
    get = read <$> get
    put = put . show
