module Make
    (module MakeData,
     MakeOutcome,
     resolveMake,
     executeMake)
    where

import MakeData
import DB
import Tool
import ToolData
import Data.List

data MakeOutcome = MakeSuccess CreatureRef Tool [ToolRef] | MakeFailed

resolveMake :: (DBReadable db) => CreatureRef -> PrepareMake -> db MakeOutcome
resolveMake c (PrepareMake (Just dk) (Just (ch,ch_tool_ref)) (Just (m,m_tool_ref)) (Just (g,g_tool_ref))) =
    return $ MakeSuccess c (improvised dk ch m g) [ch_tool_ref,m_tool_ref,g_tool_ref]
resolveMake _ _ = return MakeFailed

executeMake :: MakeOutcome -> DB ()
executeMake (MakeSuccess c t refs) =
    do mapM_ deleteTool $ nub refs
       _ <- dbAddTool t (Wielded c)
       return ()
executeMake MakeFailed = return ()
