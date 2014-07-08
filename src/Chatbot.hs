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
-- pairs
type ChatBot m = Auto m InMessage OutMessages
-- A simplified chat bot which only outputs messages to the channel it
-- received the incoming message from
type ChatBot' m = Auto m InMessage [String]

type Nick    = String
type Channel = String

data InMessage = InMessage { _inMessageNick   :: Nick
                           , _inMessageBody   :: String
                           , _inMessageSource :: Channel
                           , _inMessageTime   :: UTCTime
                           } deriving Show

-- Output map; the keys are channels and the values are messages to send to
-- each channel.
newtype OutMessages = OutMessages { _outMessageMap :: Map Channel [String]
                                  }

instance Monoid OutMessages where
    mempty = OutMessages M.empty
    mappend (OutMessages a) (OutMessages b) = OutMessages (M.unionWith (<>) a b)

-- config
botName :: Nick
botName = "autobot-test"

channels :: [Channel]
channels = ["#autobot-test"]

chatbotFP :: FilePath
chatbotFP = "data/save/chatbot"

-- main
main :: IO ()
main = launchIRC chatBot $ \_ -> return ()

-- The bot!  Basically a monoid sum of smaller bots.  Note that each
-- component bot is selectively serialized.
chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ s "seen"  $ perRoom seenBot     -- seenBot, self-serializing
                  ,             perRoom karmaBot
                  , s "ann"             announceBot
                  ]
  where
    -- transforms an Auto into a self-serializing and self-reloading Auto,
    -- saving at that given filepath.
    s fp = serializing' (chatbotFP ++ "-" ++ fp)

-- Helper function to transform a Chatbot' into a Chatbot --- chatbots
-- written for single-channel intput/output to multi-channel-aware
-- input/output.
perRoom :: Monad m => ChatBot' m -> ChatBot m
perRoom cb' = proc im -> do
    outs <- cb' -< im
    id -< OutMessages $ M.singleton (_inMessageSource im) outs

-- | The Modules/bots

-- seenBot: Maintains a map of nicks and their last received message.  On
-- the command '@seen [nick]', looks up that nick and reports the last seen
-- time.
seenBot :: Monad m => ChatBot' m
seenBot = proc (InMessage nick msg _ time) -> do
    -- mkAccum: add every '(nick, time)' pair into the map
    seens <- mkAccum (\m (n, t) -> M.insert n t m) M.empty -< (nick, time)

        -- output
    let out = case words msg of
                -- if a command, look up the request and output a report.
                "@seen":req:_ ->
                  [ case M.lookup req seens of
                      Just t  -> "'" ++ req ++ "' last seen at " ++ show t ++ "."
                      Nothing -> "No record of '" ++ req ++ "'." ]
                -- otherwise, nothing.
                _             ->
                  mzero

    -- send it out as the Auto output
    id -< out

-- karmaBot: Maintains a map of nicks and associated "karma" --- users can
-- increase a nick's karma by saying '@addKarma [nick]`...can subtract by
-- saying '@subKarma [nick]'...or just query the map by saying '@karma
-- [nick]'.  In all cases, the current karma is reported.
karmaBot :: Monad m => ChatBot' m
karmaBot = proc (InMessage _ msg _ _) -> do
    -- a karmaBlip is 'Blip (Nick, Int)' that occurs whenever a @karma
    -- command is registered.  The nick is the nick to be changed, and the
    -- Int is the change in karma.
    karmaBlip <- emitJusts comm -< msg

    -- maintain the nick-karma map, by "scanning" over every karmaBlip
    karmas <- scanB (\m (n, c) -> M.insertWith (+) n c m) M.empty -< karmaBlip

        -- output event -- tag a karmaBlip with a report to the chatroom,
        -- using the Functor instance.
    let outBlip = fmap (\(nick, _) ->
                    let karm = M.findWithDefault 0 nick karmas
                    in  ["'" ++ nick ++ "' has a karma of " ++ show karm ++ "."]
                    ) karmaBlip

    -- If 'outBlip' (the message) is there, return it.  Otherwise, return
    -- 'mzero' (empty list)
    fromBlips mzero -< outBlip
  where
    -- detect a command from the given input string.  Output 'Maybe
    -- (Nick, Int)' -- the nick to change the karma of, and the Int is the
    -- change in karma.
    comm :: String -> Maybe (Nick, Int)
    comm msg = case words msg of
                 "@addKarma":nick:_ -> Just (nick, 1 )
                 "@subKarma":nick:_ -> Just (nick, -1)
                 "@karma":nick:_    -> Just (nick, 0)
                 _                  -> Nothing

-- announceBot: Listen on all channels (including private messages) for
-- announcements of the form '@ann [message]'; when received, broadcast the
-- message to all channels the bot is in.  However, rate-limit the
-- broadcasts and only allow 3 announcements per day per user, reset every
-- day at midnight.
announceBot :: forall m. Monad m => ChatBot m
announceBot = proc im@(InMessage _ _ src time) -> do
    -- annBlip, a 'Blip (Nick, String)' (the nick sending the
    -- announcement, and the announcement), that is fired/emitted every
    -- time an announcement is observed anywhere.
    annBlip <- emitJusts announcing -< im

    -- newDay is a Blip that occurs every time a new day is observed in the
    -- InMessage's time field.
    newDay  <- onChange             -< utctDay time

        -- A Blip of the nick sending the announcement.
    let annNick = fst <$> annBlip

    -- Maintain a map of nicks and the number of times they have made
    -- announcements.  'resetOn counter' basically behaves like 'counter',
    -- which takes 'annNick' --- but when it gets a 'newDay' Blip, resets
    -- the counter Auto.
    -- The counter auto is in the 'where' clause, and just increments the
    -- entry for the incoming 'annNick' by 1 every announcement.
    amnts   <- resetOn counter -< (annNick, newDay)

        -- the response.  If allowed, broadcasts the announcement to every
        -- channel on the 'channels' list config variable.  If not allowed
        -- (because of flooding), only outputs a "No Flooding!" message to
        -- the channel where the announcement request was received.
        -- Wraps it all up in the 'OutMessages' container.
    let outmsgs = fmap (\(nick, ann) ->
                    let amt  = M.findWithDefault 0 nick amnts
                        msgs | amt <= floodLimit = (, [ann]) <$> channels
                             | otherwise         = [(src, ["No Flooding!"])]
                    in  OutMessages (M.fromList msgs)
                    ) annBlip

    -- on non-blips, output 'mempty' (an 'OutMessages' map w/ no messages)
    fromBlips mempty -< outmsgs
  where
    floodLimit :: Int
    floodLimit = 3
    -- Look for commands making announcements; returns 'Maybe (Nick,
    -- String)' --- the nick making the announcement, and the announcement.
    announcing :: InMessage -> Maybe (Nick, String)
    announcing (InMessage nick msg _ _) =
        case words msg of
          "@ann":ann -> Just (nick, nick ++ " says, \"" ++ unwords ann ++ "\".")
          _          -> Nothing
    -- nothing fancy; listens for incoming nicks and increments a nick-Int
    -- map on for every blip.
    counter :: Auto m (Blip Nick) (Map Nick Int)
    counter = scanB (\m n -> M.insertWith (+) n 1 m) M.empty

-- | Serialize instances for the time types.
instance Serialize UTCTime where
    get = read <$> get      -- haha don't do this in real life.
    put = put . show

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

-- | Boring low-level MIRC stuff; mostly stuff from the simple-irc library.
-- Don't worry about this too much :)  For more information, read the
-- simple-irc documentation.

-- ircConf: IRC configuration for simple-irc.  Specifies server, name,
-- channels, and the Privmsg handler.
ircConf :: MVar (ChatBot IO) -> IrcConfig
ircConf a = (mkDefaultConfig "irc.freenode.org" botName)
              { cChannels = channels
              , cEvents   = [Privmsg (onMessage a)]
              }

-- begin the IRC process with stdout logging.  Offer a handler with the
-- final ChatBot, in the event of an interrupt or crash.
launchIRC :: ChatBot IO -> (ChatBot IO -> IO ()) -> IO ()
launchIRC a0 cont = do
    amvr <- newMVar a0
    let launch = connect (ircConf amvr) False True
    void launch `catch` \(_ :: AsyncException) -> withMVar amvr cont

-- onMessage: the Privmsg handler.  On every Privmsg, pull the Auto out of
-- the MVar, step it to get the results, and put the modified Auto back
-- into the MVar.  Check out the documentation for simple-irc and
-- 'Control.Concurrent' for more information.
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

