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

-- | Types

-- A Chat bot; takes in a message and outputs a map of (Channel, Messages)
--   pairs
type ChatBot m = Auto m InMessage OutMessages
-- A simplified chat bot which only outputs messages to the channel it
--   received the incoming message from
type ChatBot' m = Auto m InMessage (Blip [Message])

type Nick    = String
type Channel = String
type Message = String

data InMessage = InMessage { _inMessageNick   :: Nick
                           , _inMessageBody   :: Message
                           , _inMessageSource :: Channel
                           , _inMessageTime   :: UTCTime
                           } deriving Show

-- Output map; the keys are channels and the values are messages to send to
--   each channel.
data OutMessages = OutMessages (Map Channel [Message])

instance Monoid OutMessages where
    mempty  = OutMessages M.empty
    mappend (OutMessages m1) (OutMessages m2)
            = OutMessages (M.unionWith (++) m1 m2)


-- config
botName :: Nick
botName = "autobot-test"

channels :: [Channel]
channels = ["#autobot-test"]

chatbotFP :: FilePath
chatbotFP = "data/save/chatbot"

-- main
main :: IO ()
main = launchIRC chatBot

-- The bot!  Basically a monoid sum of smaller bots.  Note that each
--   component bot is selectively serialized.
chatBot :: ChatBot IO
chatBot = mconcat [ s "seen"  $ perRoom seenBot     -- seenBot, self-serializing
                  ,             perRoom karmaBot
                  , s "ann"             announceBot
                  ]
  where
    -- transforms an Auto into a self-serializing and self-reloading Auto,
    --   saving at that given filepath.
    s fp = serializing' (chatbotFP ++ "-" ++ fp)

-- Helper function to transform a Chatbot' into a Chatbot --- chatbots
--   written for single-channel intput/output to multi-channel-aware
--   input/output.
perRoom :: Monad m => ChatBot' m -> ChatBot m
perRoom cb' = proc im -> do
    outs <- fromBlips [] . cb' -< im
    id -< OutMessages $ M.singleton (_inMessageSource im) outs

-- | The Modules/bots

-- seenBot: Maintains a map of nicks and their last received message.  On
--   the command '@seen [nick]', looks up that nick and reports the last
--   seen time.
seenBot :: Monad m => ChatBot' m
seenBot = proc (InMessage nick msg _ time) -> do
    -- seens :: Map Nick UTCTime
    -- Map containing last time each nick has spoken.  Uses `accum` and the
    --   helper function `addToMap` to update the Map on every message.
    seens <- accum addToMap M.empty -< (nick, time)

    -- query :: Blip Nick
    -- blip stream emits whenever someone queries for a last time seen;
    -- emits with the nick queried for
    query <- emitJusts getRequest -< words msg

        -- a function to get a response from a nick query
    let respond :: Nick -> [Message]
        respond qry = case M.lookup qry seens of
                        Just t  -> [qry ++ " last seen at " ++ show t ++ "."]
                        Nothing -> ["No record of " ++ qry ++ "."]

    -- output is, whenever the `query` stream emits, map `respond` to it.
    id -< respond <$> query
  where
    addToMap :: Map Nick UTCTime -> (Nick, UTCTime) -> Map Nick UTCTime
    addToMap mp (nick, time) = M.insert nick time mp
    getRequest ("@seen":request:_) = Just request
    getRequest _                   = Nothing

-- karmaBot: Maintains a map of nicks and associated "karma", imaginary
--   internet points. --- users can increase a nick's karma by saying
--   `@addKarma [nick]`...can subtract by saying `@subKarma [nick]`...or
--   just query the map by saying `@karma [nick]`.  In all cases, the
--   current karma is reported.
karmaBot :: Monad m => ChatBot' m
karmaBot = proc (InMessage _ msg _ _) -> do
    -- karmaBlip :: Blip (Nick, Int)
    -- blip stream emits when someone modifies karma, with nick and increment
    karmaBlip <- emitJusts getComm -< msg

    -- karmas :: Map Nick Int
    -- keeps track of the total karma for each user by updating with karmaBlip
    karmas    <- scanB updateMap M.empty -< karmaBlip

    -- function to look up a nick, if one is asked for
    let lookupKarma :: Nick -> [Message]
        lookupKarma nick = let karm = M.findWithDefault 0 nick karmas
                           in  [nick ++ " has a karma of " ++ show karm ++ "."]

    -- output is, whenever `karmaBlip` stream emits, look up the result
    id -< lookupKarma . fst <$> karmaBlip
  where
    getComm :: String -> Maybe (Nick, Int)
    getComm msg = case words msg of
                    "@addKarma":nick:_ -> Just (nick, 1 )
                    "@subKarma":nick:_ -> Just (nick, -1)
                    "@karma":nick:_    -> Just (nick, 0)
                    _                  -> Nothing
    updateMap :: Map Nick Int -> (Nick, Int) -> Map Nick Int
    updateMap mp (nick, change) = M.insertWith (+) nick change mp

-- announceBot: Listen on all channels (including private messages) for
--   announcements of the form `@ann [message]`; when received, broadcast
--   the message to all channels the bot is in.  However, rate-limit the
--   broadcasts and only allow 3 announcements per day per user, reset
--   every day at midnight.
announceBot :: forall m. Monad m => ChatBot m
announceBot = proc (InMessage nick msg src time) -> do
    -- annBlip :: Blip [Message]
    -- blip stream emits when someone wants an echo, with the message
    annBlip <- emitJusts getAnnounce -< (nick, msg)

    -- newDayBlip :: Blip UTCTime
    -- blip stream emits whenever the day changes
    newDayBlip <- onChange           -< utctDay time

    -- annCounts :: Map Nick Int
    -- `countEchos` counts the number of times each user asks for an echo, and
    -- `resetOn` makes it "reset" itself whenever `newDayBlip` emits.
    annCounts <- resetOn countAnns -< (nick <$ annBlip, newDayBlip)

        -- has this user flooded today...?
    let hasFlooded = M.lookup nick annCounts > Just floodLimit
        -- toRooms :: Blip [Message]
        -- blip stream emits whenever someone asks for an echo, limiting flood
        outMsg  | hasFlooded = ["No flooding!"] <$ annBlip
                | otherwise  = annBlip
        -- targets :: [Channel]
        -- the rooms to announce to.  if flooded, only echo back to source.
        targets | hasFlooded = [src]
                | otherwise  = channels

        -- outputs :: Blip (Map Channel [Message])
        -- blip stream that emits a map of messages to send to each
        -- channel.
        outputs = M.fromList . zip targets . repeat <$> outMsg

    -- when 'outputs' is not emitting, just pop out an empty 'OutMessages'.
    -- Otherwise, make one from the 'Map' that was emitted.
    fromBlipsWith mempty OutMessages -< outputs
  where
    getAnnounce :: (Nick, Message) -> Maybe [Message]
    getAnnounce (nick, msg) =
        case words msg of
          "@ann":ann -> Just [nick ++ " says \"" ++ unwords ann ++ "\"."]
          _          -> Nothing
    floodLimit = 5
    isEcho msg = case words msg of
                   "@echo":xs -> Just [unwords xs]
                   _          -> Nothing
    countAnns :: Auto m (Blip Nick) (Map Nick Int)
    countAnns = scanB countingFunction M.empty
    countingFunction :: Map Nick Int -> Nick -> Map Nick Int
    countingFunction mp nick = M.insertWith (+) nick 1 mp

-- | Serialize instances for the time types.
instance Serialize UTCTime where
    get = read <$> get      -- haha don't do this in real life.
    put = put . show

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

-- | Boring low-level MIRC stuff; mostly stuff from the simpleirc library.
--   Don't worry about this too much :)  For more information, read the
--   simpleirc documentation.

-- ircConf: IRC configuration for simpleirc.  Specifies server, name,
--   channels, and the Privmsg handler.
ircConf :: MVar (ChatBot IO) -> IrcConfig
ircConf a = (mkDefaultConfig "irc.freenode.org" botName)
              { cChannels = channels
              , cEvents   = [Privmsg (onMessage a)]
              }

-- begin the IRC process with stdout logging.
launchIRC :: ChatBot IO -> IO ()
launchIRC a0 = do
    amvr <- newMVar a0
    let launch = connect (ircConf amvr) False True
    void launch `catch` \(_ :: AsyncException) -> return ()

-- onMessage: the Privmsg handler.  On every Privmsg, pull the Auto out of
--   the MVar, step it to get the results, and put the modified Auto back
--   into the MVar.  Check out the documentation for simpleirc and
--   `Control.Concurrent` for more information.
onMessage :: MVar (ChatBot IO) -> EventFunc
onMessage amvr server msg = do
    OutMessages resps <- modifyMVar amvr $ \a ->
      case (mNick msg, mOrigin msg) of
        (Just nick, Just orig) -> do
          t <- getCurrentTime
          let inmsg = InMessage (C8.unpack nick) (C8.unpack (mMsg msg))
                                (C8.unpack orig) t
          (out, a') <- stepAuto a inmsg
          return (a', out   )
        _   ->
          return (a , mempty)
    _ <- flip M.traverseWithKey resps $ \k v ->
        mapM_ (sendMsg server (C8.pack k) . C8.pack) v
    return ()

