{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Serialize
import Control.Auto.Switch
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Map.Strict                 (Map)
import Data.Serialize
import Data.Time
import Network.SimpleIRC
import Prelude hiding                  ((.), id)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict       as M

{-# ANN module "HLint: ignore Use const" #-}

-- types
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

-- config
botName :: String
botName = "autobot-test"

channels :: [String]
channels = ["#autobot-test"]

chatbotFP :: FilePath
chatbotFP = "data/save/chatbot"

-- main
main :: IO ()
main = launchIRC chatBot $ \_ -> return ()

-- The bot!  Basically a monoid sum of smaller bots!  Each component
-- selectively serialized.
chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ s "seen"  $ perRoom seenBot
                  ,             perRoom karmaBot
                  , s "ann"             announceBot
                  ]
  where
    s fp = serializing' ("data/save/chatbot-" ++ fp)

-- Helper function to transform chat bots written for single channels (only
-- outputting one channel output) to bots that can talk to multiple
-- channels.
perRoom :: Monad m => ChatBot' m -> ChatBot m
perRoom cb' = proc im -> do
    outs <- cb' -< im
    id -< OutMessages $ M.singleton (inMessageSource im) outs

-- | The Modules/bots

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

karmaBot :: Monad m => ChatBot' m
karmaBot = proc (InMessage _ msg _ _) -> do
    karmaBlip <- emitJusts comm -< msg
    karmas    <- scanB f mempty -< karmaBlip

    let outBlip = flip fmap karmaBlip $ \(nick, _) ->
                    let karm = M.findWithDefault 0 nick karmas
                    in  ["'" ++ nick ++ "' has a karma of " ++ show karm ++ "."]

    fromBlips mzero -< outBlip
  where
    comm :: String -> Maybe (String, Int)
    comm msg = case words msg of
                 "@addKarma":nick:_ -> Just (nick, 1 )
                 "@subKarma":nick:_ -> Just (nick, -1)
                 "@karma":nick:_    -> Just (nick, 0)
                 _                  -> Nothing
    f m (nick, change) = M.insertWith (+) nick change m

announceBot :: Monad m => ChatBot m
announceBot = proc im -> do
    annBlip <- emitJusts announcing -< im
    newDay  <- onChange             -< utctDay (inMessageTime im)

    let annName = fst <$> annBlip
    amnts   <- rSwitchF (\_ -> counter) counter -< (annName, newDay)

    let outmsgs = flip fmap annBlip $ \(nick, ann) ->
                    let amt = M.findWithDefault 0 nick amnts
                    in  if amt <= floodLimit
                          then (,) <$> channels <*> pure ann
                          else [(inMessageSource im, "No flooding!")]

    fromBlips mempty -< OutMessages . fmap (:[]) . M.fromList <$> outmsgs
  where
    counter = scanB count mempty
    floodLimit :: Int
    floodLimit = 3
    announcing :: InMessage -> Maybe (String, String)
    announcing (InMessage nick msg _ _) =
        case words msg of
          "@ann":ann -> Just (nick, nick ++ " says, \"" ++ unwords ann ++ "\".")
          _          -> Nothing
    count m nick = M.insertWith (+) nick (1 :: Int) m

-- | cheating instances
instance Serialize UTCTime where
    get = read <$> get
    put = put . show

instance Serialize Day where
    get = read <$> get
    put = put . show

-- | Boring low-level MIRC stuff
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

