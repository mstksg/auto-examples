{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

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

type ChatBot m = Auto m InMessage OutMessages
type ChatBot' m = Auto m InMessage [String]

type Nick    = String
type Channel = String

data InMessage = InMessage { _inMessageNick   :: Nick
                           , _inMessageBody   :: String
                           , _inMessageSource :: Channel
                           , _inMessageTime   :: UTCTime
                           } deriving Show

newtype OutMessages = OutMessages { _outMessageMap :: Map Channel [String] }

instance Monoid OutMessages where
    mempty = OutMessages M.empty
    mappend (OutMessages a) (OutMessages b) = OutMessages (M.unionWith (<>) a b)

botName :: Nick
botName = "autobot-test"

channels :: [Channel]
channels = ["#autobot-test"]

chatbotFP :: FilePath
chatbotFP = "data/save/chatbot"

main :: IO ()
main = launchIRC chatBot

chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ s "seen"  $ perRoom seenBot     -- seenBot, self-serializing
                  ,             perRoom karmaBot
                  , s "ann"             announceBot
                  ]
  where
    s fp = serializing' (chatbotFP ++ "-" ++ fp)

perRoom :: Monad m => ChatBot' m -> ChatBot m
perRoom cb' = proc im -> do
    outs <- cb' -< im
    id -< OutMessages $ M.singleton (_inMessageSource im) outs

seenBot :: Monad m => ChatBot' m
seenBot = proc (InMessage nick msg _ time) -> do
    seens <- accum (\m (n, t) -> M.insert n t m) M.empty -< (nick, time)
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
    karmas <- scanB (\m (n, c) -> M.insertWith (+) n c m) M.empty -< karmaBlip
    let outBlip = flip fmap karmaBlip $ \(nick, _) ->
                    let karm = M.findWithDefault 0 nick karmas
                    in  ["'" ++ nick ++ "' has a karma of " ++ show karm ++ "."]
    fromBlips mzero -< outBlip
  where
    comm :: String -> Maybe (Nick, Int)
    comm msg = case words msg of
                 "@addKarma":nick:_ -> Just (nick, 1 )
                 "@subKarma":nick:_ -> Just (nick, -1)
                 "@karma":nick:_    -> Just (nick, 0)
                 _                  -> Nothing

announceBot :: forall m. Monad m => ChatBot m
announceBot = proc im@(InMessage _ _ src time) -> do
    annBlip <- emitJusts announcing -< im
    newDay  <- onChange             -< utctDay time
    let annNick = fst <$> annBlip
    amnts   <- resetOn counter -< (annNick, newDay)
    let outmsgs = fmap (\(nick, ann) ->
                    let amt  = M.findWithDefault 0 nick amnts
                        msgs | amt <= floodLimit = (, [ann]) <$> channels
                             | otherwise         = [(src, ["No Flooding!"])]
                    in  OutMessages (M.fromList msgs)
                    ) annBlip
    fromBlips mempty -< outmsgs
  where
    floodLimit :: Int
    floodLimit = 3
    announcing (InMessage nick msg _ _) =
        case words msg of
          "@ann":ann -> Just (nick, nick ++ " says, \"" ++ unwords ann ++ "\".")
          _          -> Nothing
    counter = scanB (\m n -> M.insertWith (+) n 1 m) M.empty

instance Serialize UTCTime where
    get = read <$> get      -- haha don't do this in real life.
    put = put . show

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

ircConf :: MVar (ChatBot IO) -> IrcConfig
ircConf a = (mkDefaultConfig "irc.freenode.org" botName)
              { cChannels = channels
              , cEvents   = [Privmsg (onMessage a)]
              }

launchIRC :: ChatBot IO -> IO ()
launchIRC a0 = do
    amvr <- newMVar a0
    let launch = connect (ircConf amvr) False True
    void launch `catch` \(_ :: AsyncException) -> return ()

onMessage :: MVar (ChatBot IO) -> EventFunc
onMessage amvr server msg = do
    OutMessages resps <- modifyMVar amvr $ \a ->
      case (mNick msg, mOrigin msg) of
        (Just nick, Just orig) -> do
          t <- getCurrentTime
          let inmsg = InMessage (C8.unpack nick) (C8.unpack (mMsg msg))
                                (C8.unpack orig) t
          Output out a' <- stepAuto a inmsg
          return (a', out   )
        _   ->
          return (a , mempty)
    _ <- flip M.traverseWithKey resps $ \k v ->
        mapM_ (sendMsg server (C8.pack k) . C8.pack) v
    return ()

