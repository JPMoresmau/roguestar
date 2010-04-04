{-# LANGUAGE Arrows #-}

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
import RSAGL.FRP.FRP
import RSAGL.FRP.Time
import Strings

eventStateHeader :: (String -> Bool) -> RSAnimAX () () () () () ()
eventStateHeader stateP = proc () ->
    do genericStateHeader switchTo stateP -< ()
       acs <- driverGetAnswerA -< "action-count"
       iacs <- initial -< acs
       switchContinue -< (if acs /= iacs then Just eventMessager else Nothing,())
           where switchTo s = fromMaybe eventMessager $ lookup s messages

-- | Print messages about game events.
eventMessager :: RSAnimAX () () () () () ()
eventMessager = proc () -> 
    do eventStateHeader (isNothing . flip lookup messages) -< () 
       blockContinue -< True 

type MessageHandler a b = MaybeArrow (RSAnimAX () () () ()) a b

-- | A handler for messages from a specific event state, such as \"attack-event\".
-- Parameters are:
-- * The name of the event (\"attack-event\")
-- * The handler for the event.
messageState :: String -> MessageHandler () String -> (String,RSAnimAX () () () () () ())
messageState s actionA = (s,eventStateHeader (== s) >>> (proc () ->
    do m_string <- runMaybeArrow actionA -< Just ()
       printTextOnce -< fmap ((,) Event) m_string
       t <- threadTime -< ()
       let time_out = t > fromSeconds 3
       printTextOnce -< if time_out then Just (UnexpectedEvent,"Hmmmm . . . RogueStar is puzzled. (" ++ s ++ ")") else Nothing
       blockContinue -< isNothing m_string && not time_out))

-- | As 'messageState', but just prints a simple string.
messagePrompt :: String -> String -> (String,RSAnimAX () () () () () ())
messagePrompt s prompt = messageState s $ arr (const prompt)

-- | As 'messageState', but constructs an alternate message handler to be switched
-- via 'continueWith'.
alternateMessage :: String -> MessageHandler () String -> RSAnimAX () () () () () ()
alternateMessage s actionA = snd $ messageState s actionA

-- | Provide a default value to substitute if a computation doesn't yield a value after the specified timeout period.
timeout :: Time -> b -> MessageHandler a b -> MessageHandler a b
timeout duration default_value handler = (>>>) (extract handler) $ MaybeArrow $ frp1Context $ proc m_m_o ->
    do let m_o = fromMaybe Nothing m_m_o
       t <- threadTime -< ()
       switchContinue -< (if isNothing m_o && t > duration then Just (arr $ const $ Just default_value) else Nothing,m_m_o)
       returnA -< m_o

-- | As 'driverGetAnswerA'
answer :: String -> MessageHandler () String 
answer s = liftConst s driverGetAnswerA

-- | As 'driverGetTableA' that gets one element of the 'object-details' table.
detail :: String -> MessageHandler String String
detail field = proc unique_id ->
    MaybeArrow (arr $ maybe Nothing $ \x -> tableLookup x ("property","value") field) <<< 
                      MaybeArrow (arr (fromMaybe Nothing) <<< whenJust driverGetTableA) -< ("object-details",unique_id)

-- | Switch to an alternate message handler constructed with 'alternateMessage'.
continueWith :: MessageHandler (RSAnimAX () () () () () ()) ()
continueWith = liftJust $ switchContinue <<< arr (\x -> (x,()))

-- | Get a noun from a uid for any tool or character.
nameOf :: String -> MessageHandler () Noun
nameOf who = proc () ->
    do who_id <- answer who -< ()
       who_player <- answer "who-player" -< ()
       liftJust debugOnce <<< maybeA -< if who_player == "0" then Just "nameOf: I don't know who you are . . ." else Nothing
       returnA -< case () of
           () | who_id == who_player -> You
           () | otherwise -> Singular who_id "recreant"

data Noun = X | You | Singular { _noun_id, _noun_word :: String } deriving (Eq)

nounToString :: Noun -> String
nounToString You = "you"
nounToString (Singular _ s) = "the " ++ s
nounToString X = "it"

possessiveToString :: Noun -> String
possessiveToString You = "your"
possessiveToString (Singular _ s) = "the " ++ s ++ "'s"
possessiveToString X = "its"

possessivePronounToString :: Noun -> String
possessivePronounToString You = "your"
possessivePronounToString (Singular {}) = "its"
possessivePronounToString X = "its"

nounToUID :: MessageHandler Noun String
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

sentence :: Noun -> Noun -> Noun -> String -> String
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

recognized_events :: [String]
recognized_events = map fst messages

messages :: [(String,RSAnimAX () () () () () ())]
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
	   returnA -< sentence who_attacks X X $ "$You miss(es).",
    messageState "killed-event" $ proc () -> 
        do who_killed <- nameOf "who-killed" -< ()
	   returnA -< sentence who_killed X X "$You $have been killed.",
    messageState "weapon-overheats-event" $ proc () ->
       do who_surprised <- nameOf "who-attacks" -< ()
          player_hp_string <- playerHPString -< who_surprised
          returnA -< (if who_surprised == You then "Ouch!  " else "") ++ (sentence who_surprised X X $ "$Your weapon overheats!" ++ player_hp_string),
    messageState "weapon-explodes-event" $ proc () ->
        do who_surprised <- nameOf "who-attacks" -< ()
           weapon_type <- detail "tool-type" <<< answer "weapon-used" -< ()
           player_hp_string <- playerHPString -< who_surprised
           returnA -< sentence who_surprised X X $ (if who_surprised == You then "Ouch!  Frak!\n" else "") ++
                      "$Your weapon explodes in $(your) hand!" ++
                      (if who_surprised == You && weapon_type == "gun" 
                          then "\nAre you sure you're qualified to operate a directed energy firearm?" else "") ++
                      (if who_surprised == You && weapon_type == "sword"
                          then "\nDo you have ANY training with that thing?" else "") ++ player_hp_string,
    messageState "disarm-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           who_hit <- nameOf "who-hit" -< ()
           returnA -< sentence who_attacks who_hit X "$You disarm(s) $him!",
    messageState "sunder-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           who_hit <- nameOf "who-hit" -< ()
           returnA -< sentence who_attacks who_hit X "$You sunder(s) $his weapon!",
    messageState "heal-event" $ proc () ->
        do who_healed <- nameOf "who-event" -< ()
           player_hp_string <- playerHPString -< who_healed
           returnA -< sentence who_healed X X "$You $have been healed!" ++ player_hp_string,
    messageState "expend-tool-event" $ proc () ->
        do returnA -< "That object has been used up.",
    messagePrompt "attack" "Attack.  Direction:",
    messagePrompt "fire"   "Fire.  Direction:",
    messagePrompt "move"   "Walk.  Direction:",
    messagePrompt "jump"   "Teleport jump.  Direction:",
    messagePrompt "clear-terrain"  "Clear terrain.  Direction:",
    messagePrompt "turn"  "Turn.  Direction:"]

unarmedAttack :: RSAnimAX () () () () () ()
unarmedAttack = alternateMessage "attack-event" $ proc () ->
    do who_attacks <- nameOf "who-attacks" -< ()
       who_hit <- nameOf "who-hit" -< ()
       player_hp_string <- playerHPString -< who_hit
       returnA -< sentence who_attacks who_hit X $ "$You strike(s) $him!" ++ player_hp_string

armedAttack :: RSAnimAX () () () () () ()
armedAttack = alternateMessage "attack-event" $ proc () ->
    do weapon_used <- answer "weapon-used" -< ()
       who_attacks <- nameOf "who-attacks" -< ()
       who_hit <- nameOf "who-hit" -< ()
       weapon_type <- detail "tool-type" -< weapon_used
       player_hp_string <- playerHPString -< who_hit
       returnA -< case weapon_type of
           "gun" -> sentence who_attacks who_hit X $ "$You shoot(s) $him!" ++ player_hp_string
           "sword" -> sentence who_attacks who_hit X $ "$You hit(s) $him!" ++ player_hp_string
           _ -> sentence who_attacks who_hit X $ "$You attack(s) $him!" ++ player_hp_string

-- | Generates a string for the hit points of a creature, if that information is available.
playerHPString :: MessageHandler Noun String
playerHPString = timeout (fromSeconds 0.1) "" $ proc noun ->
    do uid <- nounToUID -< noun
       hp <- detail "hp" -< uid
       maxhp <- detail "maxhp" -< uid
       returnA -< " (" ++ hp ++ "/" ++ maxhp ++ ")"


