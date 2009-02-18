{-# LANGUAGE Arrows #-}

module AnimationEvents
    (eventMessager)
    where

import Animation
import Data.Maybe
import Control.Arrow
import AnimationExtras
import PrintTextData

eventStateHeader :: (String -> Bool) -> RSAnimA1 () () () ()
eventStateHeader = genericStateHeader switchTo
    where switchTo s = fromMaybe eventMessager $ lookup s messages

-- | Print messages about game events.
eventMessager :: RSAnimA1 () () () ()
eventMessager = proc () -> 
    do eventStateHeader (isNothing . flip lookup messages) -< () 
       blockContinue -< True 

-- | A handler for messages from a specific event state, such as \"attack-event\".
messageState :: String -> RSAnimA1 () () () (Maybe String) -> (String,RSAnimA1 () () () ())
messageState s actionA = (s,eventStateHeader (== s) >>> (proc () ->
    do m_string <- actionA -< ()
       blockContinue -< isNothing m_string
       printTextOnce -< fmap ((,) Event) m_string))

messages :: [(String,RSAnimA1 () () () ())]
messages = [
    messageState "attack-event" $ proc () -> 
        do m_weapon <- driverGetAnswerA -< "weapon-used"
	   returnA -< 
	       do weapon <- m_weapon
	          return $ case () of
		      () | weapon == "0" -> "It attacks!\nIt hits!"
		      () | otherwise -> "You attack!\nYou hit!",
    messageState "miss-event" $ proc () -> 
        do m_weapon <- driverGetAnswerA -< "weapon-used"
	   returnA -<
	       do weapon <- m_weapon
	          return $ case () of
		      () | weapon == "0" -> "It attacks!\nIt misses."
		      () | otherwise -> "You attack!\nYou miss.",
    messageState "killed-event" $ proc () -> 
        do m_who_killed <- driverGetAnswerA -< "who-killed"
	   returnA -<
	       do who_killed <- m_who_killed
	          return $ case () of
		      () | who_killed == "2" -> "You are mortally wounded."
		      () | otherwise -> "You kill it!",
    messageState "weapon-overheats-event" $ proc () ->
       do m_who_surprised <- driverGetAnswerA -< "who-attacks"
          returnA -<
              do who_surprised <- m_who_surprised
                 return $ case () of
                     () | who_surprised == "2" -> "You attack!\nOuch!  Your weapon overheats!"
                     () | otherwise -> "It attacks!\nIt's weapon overheats!",
    messageState "weapon-explodes-event" $ proc () ->
        do m_who_surprised <- driverGetAnswerA -< "who-attacks"
           returnA -<
               do who_surprised <- m_who_surprised
                  return $ case () of
                      () | who_surprised == "2" -> "You attack!\nYour weapon explodes in your hand!\nAre you sure you're even qualified to operate a directed energy firearm?"
                      () | otherwise -> "It attacks!\nIts weapon explodes in its hands!",
    messageState "disarm-event" $ proc () ->
        do m_who_attacks <- driverGetAnswerA -< "who-attacks"
           returnA -<
               do who_attacks <- m_who_attacks
                  return $ case () of
                      () | who_attacks == "2" -> "You attack!\nYou disarm it!"
                      () | otherwise -> "It attacks!\nIt disarms you!",
    messageState "sunder-event" $ proc () ->
        do m_who_attacks <- driverGetAnswerA -< "who-attacks"
           returnA -<
               do who_attacks <- m_who_attacks
                  return $ case () of
                      () | who_attacks == "2" -> "You attack!\nYou sunder it's weapon!"
                      () | otherwise -> "It attacks!\nIt sunders your weapon!"]

