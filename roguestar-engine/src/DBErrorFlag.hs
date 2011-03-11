module DBErrorFlag
    (ErrorFlag(..))
    where

data ErrorFlag =
    BuildingApproachWrongAngle -- some buildings (like stargates) are sensitive to the angle of approach
  | NothingAtFeet              -- tried to pick something up, but there is nothing at your feet
  | NothingInInventory         -- tried to perform an inventory action, but your inventory is empty
  | NotStanding                -- the player is not standing on anything (e.g. species selection state)
  | NoStargateAddress          -- tried to move through the stargate network, but there was no destination
  | NoToolWielded              -- tried to perform an action that requires a wielded tool
  | ToolIs_NotAtFeet           -- tried to pick something up, but it isn't at your feet
  | ToolIs_NotInInventory      -- tried to perform an inventory action on a tool that isn't in inventory
  | ToolIs_NotWieldable        -- tried to wield a tool that can't be wielded.  As of March 2010, there are no such tools, so this is a bug.
  | ToolIs_Unreachable         -- tried to perform an action with a tool that isn't in reach (not in inventory, wielded, or at feet)
  | ToolIs_Innapropriate       -- tried to perform an action with a tool that can be used for that purpose
  | Unable                     -- you can't do it
    deriving (Eq,Ord,Read,Show)

