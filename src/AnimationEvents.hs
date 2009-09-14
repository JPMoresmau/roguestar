{-# LANGUAGE Arrows #-}

module AnimationEvents
    (eventMessager)
    where

import Animation
import Data.Maybe
import Control.Arrow
import AnimationExtras
import PrintTextData
import MaybeArrow
import Data.Char
import Data.List
import Data.Monoid
import Tables
import RSAGL.FRP.FRP
import RSAGL.FRP.Time

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
messageState :: String -> MessageHandler () String -> (String,RSAnimAX () () () () () ())
messageState s actionA = (s,eventStateHeader (== s) >>> (proc () ->
    do m_string <- runMaybeArrow actionA -< Just ()
       printTextOnce -< fmap ((,) Event) m_string
       t <- threadTime -< ()
       let time_out = t > fromSeconds 3
       printTextOnce -< if time_out then Just (UnexpectedEvent,"Hmmmm . . . RogueStar is puzzled. (" ++ s ++ ")") else Nothing
       blockContinue -< isNothing m_string && not time_out))

alternateMessage :: String -> MessageHandler () String -> RSAnimAX () () () () () ()
alternateMessage s actionA = snd $ messageState s actionA

answer :: String -> MessageHandler () String 
answer s = liftConst s driverGetAnswerA

detail :: String -> MessageHandler String String
detail field = proc unique_id ->
    MaybeArrow (arr $ maybe Nothing $ \x -> tableLookup x ("property","value") field) <<< 
                      MaybeArrow (arr (fromMaybe Nothing) <<< whenJust driverGetTableA) -< ("object-details",unique_id)

continueWith :: MessageHandler (RSAnimAX () () () () () ()) ()
continueWith = liftJust $ switchContinue <<< arr (\x -> (x,()))

nameOf :: String -> MessageHandler () Noun
nameOf who = proc () ->
    do who_id <- answer who -< ()
       who_player <- answer "who-player" -< ()
       liftJust debugOnce <<< maybeA -< if who_player == "0" then Just "nameOf: I don't know who you are . . ." else Nothing
       returnA -< case () of
           () | who_id == who_player -> You
           () | otherwise -> Singular "recreant"

data Noun = X | You | Singular String deriving (Eq)

nounToString :: Noun -> String
nounToString You = "you"
nounToString (Singular s) = "the " ++ s
nounToString X = "it"

possessiveToString :: Noun -> String
possessiveToString You = "your"
possessiveToString (Singular s) = "the " ++ s ++ "'s"
possessiveToString X = "its"

possessivePronounToString :: Noun -> String
possessivePronounToString You = "your"
possessivePronounToString (Singular _) = "its"
possessivePronounToString X = "its"

isPlural :: Noun -> Bool
isPlural You = True
isPlural (Singular {}) = False
isPlural X = False

replace :: String -> String -> String -> String
replace _ _ [] = []
replace a b s = case stripPrefix a s of
    Nothing -> head s : replace a b (tail s)
    Just s' -> b ++ replace a b s'

capitalize :: String -> String
capitalize [] = []
capitalize (s:ss) = toUpper s : ss

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
	   returnA -< sentence who_attacks X X "$You miss(es).",
    messageState "killed-event" $ proc () -> 
        do who_killed <- nameOf "who-killed" -< ()
	   returnA -< sentence who_killed X X "$You $have been killed.",
    messageState "weapon-overheats-event" $ proc () ->
       do who_surprised <- nameOf "who-attacks" -< ()
          returnA -< (if who_surprised == You then "Ouch!  " else "") ++ sentence who_surprised X X "$Your weapon overheats!",
    messageState "weapon-explodes-event" $ proc () ->
        do who_surprised <- nameOf "who-attacks" -< ()
           weapon_type <- detail "tool-type" <<< answer "weapon-used" -< ()
           returnA -< sentence who_surprised X X $ (if who_surprised == You then "Ouch!  Frak!\n" else "") ++
                      "$Your weapon explodes in $(your) hand!" ++
                      (if who_surprised == You && weapon_type == "gun" 
                          then "\nAre you sure you're qualified to operate a directed energy firearm?" else "") ++
                      (if who_surprised == You && weapon_type == "sword"
                          then "\nDo you have ANY training with that thing?" else ""),
    messageState "disarm-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           who_hit <- nameOf "who-hit" -< ()
           returnA -< sentence who_attacks who_hit X "$You disarm(s) $him!",
    messageState "sunder-event" $ proc () ->
        do who_attacks <- nameOf "who-attacks" -< ()
           who_hit <- nameOf "who-hit" -< ()
           returnA -< sentence who_attacks who_hit X "$You sunder(s) $his weapon!"]

unarmedAttack :: RSAnimAX () () () () () ()
unarmedAttack = alternateMessage "attack-event" $ proc () ->
    do who_attacks <- nameOf "who-attacks" -< ()
       who_hit <- nameOf "who-hit" -< ()
       returnA -< sentence who_attacks who_hit X "$You strike(s) $him!"

armedAttack :: RSAnimAX () () () () () ()
armedAttack = alternateMessage "attack-event" $ proc () ->
    do weapon_used <- answer "weapon-used" -< ()
       who_attacks <- nameOf "who-attacks" -< ()
       who_hit <- nameOf "who-hit" -< ()
       weapon_type <- detail "tool-type" -< weapon_used
       --weapon_name <- detail "tool" -< weapon_used
       returnA -< case weapon_type of
           "gun" -> sentence who_attacks who_hit X "$You shoot(s) $him!"
           "sword" -> sentence who_attacks who_hit X "$You hit(s) $him!"
           _ -> sentence who_attacks who_hit X "$You attack(s) $him!"

