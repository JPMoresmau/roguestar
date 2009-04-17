{-# LANGUAGE Arrows #-}

module AnimationMenus
    (menu_states,
     menuDispatch)
    where

import AnimationExtras
import Animation
import PrintText
import RSAGL.FRP
import Control.Arrow
import Strings
import Tables
import Data.Monoid
import Data.Maybe
import Actions
import Scene

-- | States in which we try to display a menu.
menu_states :: [String]
menu_states = ["race-selection",
               "class-selection",
               "pickup",
               "drop",
               "wield"]

-- Header for menu states.  This will automatically switch away to an approprate menu if the provided state predicate does not match.
menuStateHeader :: (String -> Bool) -> RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuStateHeader f = genericStateHeader switchTo f >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)
  where switchTo "race-selection" = menuRaceSelection
        switchTo "class-selection" = menuClassSelection
        switchTo "pickup" = toolMenuSelection
        switchTo "drop" = toolMenuSelection
        switchTo "wield" = toolMenuSelection
        switchTo unknown_state = menuStateHeader (== unknown_state)

menuDispatch :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuDispatch = menuStateHeader (const False) >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)

menuRaceSelection :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuRaceSelection = proc s -> 
    do result <- menuStateHeader (== "race-selection") -< s
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< ()
       printMenuA select_race_action_names -< ()
       printTextA -< Just (Query,"Select a Race:")
       returnA -< result

menuClassSelection :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
menuClassSelection = proc () -> 
    do result <- menuStateHeader (== "class-selection") -< ()
       changed <- edgep <<< sticky isJust Nothing <<< arr (fmap table_created) <<< driverGetTableA -< ("player-stats","0")
       switchContinue -< (if changed then Just menuClassSelection else Nothing,())
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< ()
       printCharacterStats 0 -< ()
       printMenuA select_base_class_action_names -< ()
       printMenuItemA "reroll" -< ()
       printTextA -< Just (Query,"Select a Class:")
       returnA -< result

printCharacterStats :: Integer -> RSAnimAX any t i o () ()
printCharacterStats unique_id = proc () ->
    do m_player_stats <- driverGetTableA -< ("player-stats",show unique_id)
       print1CharacterStat -< (m_player_stats,"str")
       print1CharacterStat -< (m_player_stats,"spd")
       print1CharacterStat -< (m_player_stats,"con")
       print1CharacterStat -< (m_player_stats,"int")
       print1CharacterStat -< (m_player_stats,"per")
       print1CharacterStat -< (m_player_stats,"cha")
       print1CharacterStat -< (m_player_stats,"mind")

print1CharacterStat :: RSAnimAX any t i o (Maybe RoguestarTable,String) ()
print1CharacterStat = proc (m_player_stats,stat_str) ->
    do let m_stat_int = (\x -> tableLookupInteger x ("property","value") stat_str) =<< m_player_stats
       printTextA -< fmap (\x -> (Event,hrstring stat_str ++ ": " ++ show x)) m_stat_int

toolMenuSelection :: RSAnimA1 () SceneLayerInfo () SceneLayerInfo
toolMenuSelection = proc () ->
    do menuStateHeader (`elem` ["pickup","drop","wield"]) -< ()
       state <- sticky isJust Nothing <<< driverGetAnswerA -< "menu-state"
       m_menu_data <- sticky isJust Nothing <<< driverGetTableA -< ("menu","7")
       menu_state <- sticky isJust Nothing <<< driverGetAnswerA -< "menu-state"
       clearPrintTextA -< ()
       requestPrintTextMode -< Unlimited
       printTextA -< Just (Query, unlines $ flip (maybe []) m_menu_data $ \menu_data -> flip map (tableSelect menu_data ["n","name"]) $ \[n,name] ->
           case Just n == menu_state of
               True ->  " ---> " ++ hrstring name
               False -> "      " ++ hrstring name)
       printTextA -< Just (Query, case state of
           Just "pickup" -> "Select an item to pick up: "
           Just "drop" -> "Select an item to drop: "
           Just "wield" -> "Select an item to wield: "
           _ -> "Select an item: ")
       printMenuItemA "next" -< ()
       printMenuItemA "prev" -< ()
       printMenuItemA "escape" -< ()
       returnA -< roguestarSceneLayerInfo mempty basic_camera
