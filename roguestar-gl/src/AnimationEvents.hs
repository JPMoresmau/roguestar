{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, RankNTypes, FlexibleContexts #-}

module AnimationEvents
    (eventMessager,recognized_events)
    where

import Animation
import Data.Maybe
import Control.Arrow
import AnimationExtras
import PrintTextData
import MaybeArrow
import Data.Monoid
import Tables
import RSAGL.FRP
import Strings
import qualified Data.ByteString.Char8 as B

eventStateHeader :: (FRPModel m) => (B.ByteString -> Bool) -> EventHandler e m () ()
eventStateHeader stateP = proc () ->
    do genericStateHeader switchTo stateP -< ()
       acs <- driverGetAnswerA -< "action-count"
       iacs <- initial -< acs
       switchContinue -< (if acs /= iacs then Just eventMessager else Nothing,())
           where switchTo s = fromMaybe eventMessager $ lookup s messages

type EventSwitch m = RSwitch Disabled () () () m
type EventHandler e m a b = FRP e (EventSwitch m) a b
type MessageHandler e m a b = MaybeArrow (FRP e (EventSwitch m)) a b

-- | Print messages about game events.
eventMessager :: (FRPModel m) => EventHandler e m () ()
eventMessager = proc () ->
    do eventStateHeader (not . (`elem` recognized_events)) -< ()
       blockContinue -< True

-- | A handler for messages from a specific event state, such as \"attack-event\".
-- Parameters are:
-- * The name of the event (\"attack-event\")
-- * The handler for the event.
messageState :: (FRPModel m) =>
                B.ByteString ->
                MessageHandler e m () (TextType,B.ByteString) ->
                (B.ByteString,EventHandler e m () ())
messageState s actionA = (s,eventStateHeader (== s) >>> (proc () ->
    do m_string <- runMaybeArrow actionA -< Just ()
       printTextOnce -< m_string
       t <- threadTime -< ()
       let time_out = t > fromSeconds 3
       printTextOnce -< if time_out then Just (UnexpectedEvent,"Hmmmm . . . RogueStar is puzzled. (" `B.append` s `B.append` ")") else Nothing
       blockContinue -< isNothing m_string && not time_out))

-- | As 'messageState', but just prints a simple string.
messagePrompt :: (FRPModel m) => B.ByteString -> B.ByteString -> (B.ByteString,EventHandler e m () ())
messagePrompt s prompt = messageState s $ arr (const (Query,prompt))

-- | As 'messageState', but constructs an alternate message handler to be
-- switched via 'continueWith'.
alternateMessage :: (FRPModel m) =>
                    B.ByteString ->
                    MessageHandler e m () (TextType,B.ByteString) ->
                    EventHandler e m () ()
alternateMessage s actionA = snd $ messageState s actionA

-- | Provide a default value to substitute if a computation doesn't yield a value after the specified timeout period.
timeout :: (FRPModel m) => Time -> b -> MessageHandler e m a b -> MessageHandler e m a b
timeout duration default_value handler = (>>>) (extract handler) $ MaybeArrow $ frp1Context $ proc m_m_o ->
    do let m_o = fromMaybe Nothing m_m_o
       t <- threadTime -< ()
       switchContinue -< (if isNothing m_o && t > duration then Just (arr $ const $ Just default_value) else Nothing,m_m_o)
       returnA -< m_o

-- | As 'driverGetAnswerA'
answer :: B.ByteString -> MessageHandler e m () B.ByteString
answer s = liftConst s driverGetAnswerA

-- | As 'driverGetTableA' that gets one element of the 'object-details' table.
detail :: (FRPModel m) => B.ByteString -> MessageHandler e m B.ByteString B.ByteString
detail field = proc unique_id ->
    MaybeArrow (arr $ maybe Nothing $ \x -> tableLookup x ("property","value") field) <<< 
                      MaybeArrow (arr (fromMaybe Nothing) <<< whenJust driverGetTableA) -< ("object-details",unique_id)

-- | Switch to an alternate message handler constructed with 'alternateMessage'.
continueWith :: (FRPModel m) => MessageHandler e m (EventHandler e m () ()) ()
continueWith = liftJust $ switchContinue <<< arr (\x -> (x,()))

-- | Get a noun from a uid for any tool or character.
nameOf :: (FRPModel m) => B.ByteString -> MessageHandler e m () Noun
nameOf who = proc () ->
    do who_id <- answer who -< ()
       who_player <- answer "who-player" -< ()
       species_name <- detail "species" -< who_id
       liftJust debugOnce <<< maybeA -< if who_player == "0" then Just "nameOf: I don't know who you are . . ." else Nothing
       returnA -< case () of
           () | who_id == who_player -> You
           () | otherwise -> Singular who_id $ hrstring species_name

data Noun = X | You | Singular { _noun_id, _noun_word :: B.ByteString } deriving (Eq)

nounToString :: Noun -> B.ByteString
nounToString You = "you"
nounToString (Singular _ s) = "the " `B.append` s
nounToString X = "it"

possessiveToString :: Noun -> B.ByteString
possessiveToString You = "your"
possessiveToString (Singular _ s) = B.concat ["the ",s,"'s"]
possessiveToString X = "its"

possessivePronounToString :: Noun -> B.ByteString
possessivePronounToString You = "your"
possessivePronounToString (Singular {}) = "its"
possessivePronounToString X = "its"

nounToUID :: MessageHandler e m Noun B.ByteString
nounToUID = proc noun ->
    do you_id <- answer "who-player" -< ()
       returnA -< case noun of
           X -> "0"
           You -> you_id
           Singular uid _ -> uid

isPlural :: Noun -> Bool
isPlural You = True
isPlural (Singular {}) = False
isPlural X = False

sentence :: Noun -> Noun -> Noun -> B.ByteString -> B.ByteString
sentence subject he1 he2 = appEndo $ mconcat $ map Endo $
              he he2 ++ [replace "%" "$"] ++ he he1 ++
              [replace "$you" $ nounToString subject,
               replace "$You" $ capitalize $ nounToString subject,
               replace "$your" $ possessiveToString subject,
               replace "$Your" $ capitalize $ possessiveToString subject,
               replace "$(your)" $ possessivePronounToString subject,
               replace "$(Your)" $ capitalize $ possessivePronounToString subject,
               replace "(s)" $ if isPlural subject then "" else "s",
               replace "(es)" $ if isPlural subject then "" else "es",
               replace "$have" $ if isPlural subject then "have" else "has",
               replace "$Have" $ if isPlural subject then "Have" else "has"]
   where he obj = [replace "$he" $ nounToString obj,
                   replace "$He" $ capitalize $ nounToString obj,
                   replace "$him" $ nounToString obj,
                   replace "$Him" $ capitalize $ nounToString obj,
                   replace "$his" $ possessiveToString obj,
                   replace "$His" $ capitalize $ possessiveToString obj,
                   replace "$(his)" $ possessivePronounToString obj,
                   replace "$(His)" $ capitalize $ possessivePronounToString obj]

recognized_events :: [B.ByteString]
recognized_events = map fst (messages :: [(B.ByteString,EventHandler e () () ())])

messages :: (FRPModel m) => [(B.ByteString,EventHandler e m () ())]
messages = [
    messageState "attack-event" $ proc () ->
        do weapon_used <- answer "weapon-used" -< ()
           continueWith -< if weapon_used == "0"
                               then unarmedAttack
                               else armedAttack
           guardA -< False
           returnA -< error "messageState: \"attack-event\" unreachable",
    messageState "miss-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           returnA -< (Event,sentence who_attacks X X $ "$You miss(es)."),
    messageState "killed-event" $ proc () ->
        do who_killed <- nameOf "who-killed" -< ()
           returnA -< (Event,sentence who_killed X X "$You $have been killed."),
    messageState "weapon-overheats-event" $ proc () ->
       do who_surprised <- nameOf "who-attacks" -< ()
          player_hp_string <- playerHPString -< who_surprised
          returnA -< ((,) Event) $
              (if who_surprised == You then "Ouch!  " else "") `B.append`
              (sentence who_surprised X X $
                  "$Your weapon overheats!" `B.append` player_hp_string),
    messageState "weapon-explodes-event" $ proc () ->
        do who_surprised <- nameOf "who-attacks" -< ()
           weapon_type <- detail "tool-type" <<< answer "weapon-used" -< ()
           player_hp_string <- playerHPString -< who_surprised
           returnA -< ((,) Event) $ sentence who_surprised X X $
               (if who_surprised == You then "Ouch!  Frak!\n" else "")
                   `B.append`
               "$Your weapon explodes in $(your) hand!"
                   `B.append`
               (if who_surprised == You && weapon_type == "gun"
                    then "\nAre you sure you're qualified to operate a "
                             `B.append`
                         "directed energy firearm?"
                    else "")
                   `B.append`
               (if who_surprised == You && weapon_type == "sword"
                    then "\nDo you have ANY training with that thing?" else "")
                   `B.append`
               player_hp_string,
    messageState "disarm-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           who_hit <- nameOf "who-hit" -< ()
           returnA -< (Event,
               sentence who_attacks who_hit X "$You disarm(s) $him!"),
    messageState "sunder-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           who_hit <- nameOf "who-hit" -< ()
           returnA -< (Event,
               sentence who_attacks who_hit X "$You sunder(s) $his weapon!"),
    messageState "heal-event" $ proc () ->
        do who_healed <- nameOf "who-event" -< ()
           player_hp_string <- playerHPString -< who_healed
           returnA -< (Event,
               sentence who_healed X X "$You $have been healed!"
                   `B.append`
               player_hp_string),
    messageState "expend-tool-event" $ proc () ->
        do returnA -< (Update,"That object has been used up."),
    messageState "bump-event" $ proc () ->
        do new_level <- answer "new-level" -< ()
           new_class <- answer "new-character-class" -< ()
           returnA -< (Event,
               case (new_level,new_class) of
                   ("nothing","nothing") -> "You feel one step closer to your goal."
                   ("nothing","starchild") -> "It's full of stars."
                   (_,"nothing") -> "Welcome to level " `B.append` new_level `B.append` "."
                   (_,_) -> "Roguestar is confused by this bump event."),
    messagePrompt "attack" "Attack.  Direction:",
    messagePrompt "fire"   "Fire.  Direction:",
    messagePrompt "move"   "Walk.  Direction:",
    messagePrompt "jump"   "Teleport jump.  Direction:",
    messagePrompt "clear-terrain"  "Clear terrain.  Direction:",
    messagePrompt "turn"  "Turn.  Direction:"]

unarmedAttack :: (FRPModel m) => EventHandler e m () ()
unarmedAttack = alternateMessage "attack-event" $ proc () ->
    do who_attacks <- nameOf "who-attacks" -< ()
       who_hit <- nameOf "who-hit" -< ()
       player_hp_string <- playerHPString -< who_hit
       returnA -< (Event,
           sentence who_attacks who_hit X $ "$You strike(s) $him!"
                                                `B.append`
                                            player_hp_string)

armedAttack :: (FRPModel m) => EventHandler e m () ()
armedAttack = alternateMessage "attack-event" $ proc () ->
    do weapon_used <- answer "weapon-used" -< ()
       who_attacks <- nameOf "who-attacks" -< ()
       who_hit <- nameOf "who-hit" -< ()
       weapon_type <- detail "tool-type" -< weapon_used
       player_hp_string <- playerHPString -< who_hit
       returnA -< case weapon_type of
           "gun" -> (Event, sentence who_attacks who_hit X $
                        "$You shoot(s) $him!" `B.append` player_hp_string)
           "sword" -> (Event, sentence who_attacks who_hit X $
                        "$You hit(s) $him!" `B.append` player_hp_string)
           _ -> (Event, sentence who_attacks who_hit X $
                    "$You attack(s) $him!" `B.append` player_hp_string)

-- | Generates a string for the hit points of a creature, if that information is available.
playerHPString :: (FRPModel m) => MessageHandler e m Noun B.ByteString
playerHPString = timeout (fromSeconds 1.0) "" $ proc noun ->
    do uid <- nounToUID -< noun
       hp <- detail "hp" -< uid
       maxhp <- detail "maxhp" -< uid
       returnA -< " (" `B.append` hp `B.append` "/" `B.append` maxhp `B.append` ")"


