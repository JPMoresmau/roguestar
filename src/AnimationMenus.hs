{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

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
import qualified Data.ByteString.Char8 as B

type MenuSwitch m = RSwitch Disabled () () SceneLayerInfo m
type MenuHandler e m = FRP e (MenuSwitch m) () SceneLayerInfo

-- Header for menu states.  This will automatically switch away to an approprate menu if the provided state predicate does not match.
menuStateHeader :: (FRPModel m) => (B.ByteString -> Bool) -> MenuHandler e m
menuStateHeader f = genericStateHeader switchTo f >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)
  where switchTo "race-selection" = menuRaceSelection
        switchTo "class-selection" = menuClassSelection
        switchTo "pickup" = toolMenuSelection
        switchTo "drop" = toolMenuSelection
        switchTo "wield" = toolMenuSelection
        switchTo "make" = toolMenuSelection
        switchTo "make-what" = makeWhatMenuSelection
        switchTo "make-finished" = makeFinishedMenuSelection
        switchTo unknown_state = menuStateHeader (== unknown_state)

menuDispatch :: (FRPModel m) => MenuHandler e m
menuDispatch = menuStateHeader (const False) >>> arr (const $ roguestarSceneLayerInfo mempty basic_camera)

menuRaceSelection :: (FRPModel m) => MenuHandler e m
menuRaceSelection = proc s -> 
    do result <- menuStateHeader (== "race-selection") -< s
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< Just ()
       printMenuA select_race_action_names -< ()
       printTextA -< Just (Query,"Select a Race:")
       returnA -< result

menuClassSelection :: (FRPModel m) => MenuHandler e m
menuClassSelection = proc () -> 
    do result <- menuStateHeader (== "class-selection") -< ()
       stats <- sticky isJust Nothing <<< arr (fmap table_created) <<< driverGetTableA -< ("player-stats","0")
       initial_stats <- initial -< stats
       let change = stats /= initial_stats
       switchContinue -< (if change then Just menuClassSelection else Nothing,())
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< Just ()
       printCharacterStats 0 -< ()
       printMenuA select_base_class_action_names -< ()
       printMenuItemA "reroll" -< ()
       printTextA -< Just (Query,"Select a Class:")
       returnA -< result

printCharacterStats :: (FRPModel m, StateOf m ~ AnimationState) => Integer -> FRP e m () ()
printCharacterStats unique_id = proc () ->
    do m_player_stats <- driverGetTableA -< ("player-stats",B.pack $ show unique_id)
       print1CharacterStat -< (m_player_stats,"str")
       print1CharacterStat -< (m_player_stats,"spd")
       print1CharacterStat -< (m_player_stats,"con")
       printTextA -< Just (Event,"-")
       print1CharacterStat -< (m_player_stats,"per")
       printTextA -< Just (Event,"-")
       print1CharacterStat -< (m_player_stats,"int")
       print1CharacterStat -< (m_player_stats,"cha")
       print1CharacterStat -< (m_player_stats,"mind")
       printTextA -< Just (Event,"-")
       print1CharacterStat -< (m_player_stats,"maxhp")
  
print1CharacterStat :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m (Maybe RoguestarTable,B.ByteString) ()
print1CharacterStat = proc (m_player_stats,stat_str) ->
    do let m_stat_int = (\x -> tableLookupInteger x ("property","value") stat_str) =<< m_player_stats
       printTextA -< fmap (\x -> (Event,hrstring stat_str `B.append` ": " `B.append` (B.pack $ show x))) m_stat_int

makeWhatMenuSelection :: (FRPModel m) => MenuHandler e m
makeWhatMenuSelection = proc () ->
    do result <- menuStateHeader (== "make-what") -< ()
       requestPrintTextMode -< Unlimited
       clearPrintTextA -< Just ()
       printMenuA make_what_action_names -< ()
       printTextA -< Just (Query,"Build what?")
       returnA -< result

makeFinishedMenuSelection :: (FRPModel m) => MenuHandler e m
makeFinishedMenuSelection = proc () ->
    do result <- menuStateHeader (== "make-finished") -< ()
       clearPrintTextA -< Just ()
       printTextA -< Just (Query,"Confirm.")
       returnA -< result

toolMenuSelection :: (FRPModel m) => MenuHandler e m
toolMenuSelection = proc () ->
    do menuStateHeader (`elem` ["pickup","drop","wield","make"]) -< ()
       state <- sticky isJust Nothing <<< driverGetAnswerA -< "menu-state"
       m_menu_data <- sticky isJust Nothing <<< driverGetTableA -< ("menu","7")
       menu_state <- sticky isJust Nothing <<< driverGetAnswerA -< "menu-state"
       clearPrintTextA -< Just ()
       requestPrintTextMode -< Unlimited
       printTextA -< Just (Query, B.unlines $ flip (maybe []) m_menu_data $ \menu_data -> flip map (tableSelect menu_data ["n","name"]) $ \[n,name] ->
           case Just n == menu_state of
               True ->  " ---> " `B.append` hrstring name
               False -> "      " `B.append` hrstring name)
       printTextA -< Just (Query, case state of
           Just "pickup" -> "Select an item to pick up: "
           Just "drop" -> "Select an item to drop: "
           Just "wield" -> "Select an item to wield: "
           Just "make" -> "Select materials to craft an item: "  -- FIXME should say what kind of item
           _ -> "Select an item: ")
       printMenuItemA "next" -< ()
       printMenuItemA "prev" -< ()
       printMenuItemA "escape" -< ()
       returnA -< roguestarSceneLayerInfo mempty basic_camera

