{-# LANGUAGE ExistentialQuantification, Arrows, ScopedTypeVariables, Rank2Types,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             UndecidableInstances, DoRec #-}

module RSAGL.FRP.FRP
    (FRP,
     switchContinue,
     switchTerminate,
     spawnThreads,
     killThreadIf,
     threadIdentity,
     withThreadIdentity,
     frpTest,
     FRPProgram,
     newFRPProgram,
     newFRP1Program,
     updateFRPProgram,
     accumulate,
     absoluteTime,
     deltaTime,
     ThreadIdentityRule,
     forbidDuplicates,
     allowAnonymous,
     nullaryThreadIdentity,
     frpContext,
     frp1Context,
     whenJust,
     ioInit,
     ioAction,
     outgoingBy,
     outgoing,
     incoming,
     randomA)
    where

import Prelude hiding ((.),id)
import RSAGL.FRP.FactoryArrow
import Control.Monad.Cont
import RSAGL.FRP.Time
import RSAGL.FRP.FRPModel
import Control.Concurrent.MVar
import Control.Category
import Control.Arrow
import Control.Arrow.Operations hiding (delay)
import Data.IORef
import Control.Applicative
import RSAGL.Math.AbstractVector
import Data.List
import Data.Maybe
import Control.Exception
import RSAGL.FRP.RecombinantState
import RSAGL.FRP.Message
import System.Random

{--------------------------------------------------------------------------------}
--    FRP Data Structures
{--------------------------------------------------------------------------------}

-- | State information for a currently-executed FRP program.
data FRPState i o = FRPState { 
                               -- | Ending time of the current frame, and the frame-local time horizon.
                               frpstate_absolute_time :: Time,
                               -- | Delta to the ending time of the previous frame.
                               frpstate_delta_time :: Time,
                              -- | Continuation to exit the current switch.
                               frpstate_exit :: (Maybe o) -> ContT (Maybe o) IO (Maybe o) }

data FRPInit s t i o = FRPInit {
    frp_current_switch :: IORef (i -> ContT (Maybe o) IO (Maybe o)),
    frp_state :: IORef (FRPState i o),
    frp_user_state :: IORef s,
    -- | Put a thread in here to spawn it.
    frp_spawned_threads :: MVar [FRPInit s t i o],
    frp_previous_time :: IORef (Maybe Time),
    frp_thread_identity :: t,
    frp_previous_result :: IORef (Maybe o) }

type FRPProgram s i o = FRPInit s () i o

-- | A switchable automata with timewise numerical methods.
newtype FRP e m j p = FRP (FRPInit (StateOf m) (ThreadIDOf m) (SwitchInputOf m) (SwitchOutputOf m) -> 
                          FactoryArrow IO (ContT (Maybe (SwitchOutputOf m)) IO) j p)

instance Functor (FRP e m j) where
    fmap f frpx = frpx >>> arr f

instance Applicative (FRP e m j) where
    pure a = proc _ -> returnA -< a
    f <*> s = proc i ->
        do s' <- s -< i
           f' <- f -< i
           returnA -< f' s'

instance Category (FRP e m) where
    (FRP a) . (FRP b) = FRP $ \frp_init -> a frp_init . b frp_init
    id = FRP $ const id

instance Arrow (FRP e m) where
    arr f = FRP $ \_ -> arr f
    first (FRP f) = FRP $ \frp_init -> first (f frp_init)
    second (FRP f) = FRP $ \frp_init -> second (f frp_init)

instance (StateOf m ~ s) => ArrowState s (FRP e m) where
    fetch = frpxOf $ \frpinit _ -> lift $ getProgramState frpinit
    store = frpxOf $ \frpinit x -> lift $ putProgramState frpinit x

-- | Construct a single-threaded FRPProgram.
newFRP1Program :: (forall e. FRP e (FRP1 s i o) i o) -> IO (FRPProgram s i o)
newFRP1Program thread = unsafeFRPProgram (error "newFRP1Program: impossible, tried to access the spawned_threads pool from a single threaded FRPProgram.") () thread

-- | Construct a multi-threaded FRPProgram.
newFRPProgram :: (RecombinantState s,Eq t) => 
                 ThreadIdentityRule t -> 
                 (forall e. [(t,FRP e (FRPX t s i o) i o)]) -> 
                 IO (FRPProgram s i [(t,o)])
newFRPProgram rule seed_threads = newFRP1Program $ frpContext rule seed_threads

-- | Construct an FRPProgram from a single seed thread.  This program will spawn threads
-- into the specified MVar.
unsafeFRPProgram :: MVar [FRPInit (StateOf m) (ThreadIDOf m) (SwitchInputOf m) (SwitchOutputOf m)] -> 
                    ThreadIDOf m -> 
                    FRP e m (SwitchInputOf m) (SwitchOutputOf m) -> 
                    IO (FRPInit (StateOf m) (ThreadIDOf m) (SwitchInputOf m) (SwitchOutputOf m))
unsafeFRPProgram spawned_threads t frpx =
    do frpstate_ref <- newIORef $ error "Tried to use uninitialized FRPState variable."
       current_switch_ref <- newIORef $ error "Tried to use uninitialized frp_current_switch variable."
       previous_time_ref <- newIORef Nothing
       previous_result_ref <- newIORef Nothing
       user_state_ref <- newIORef $ error "Tried to use uninitialized user state variable. (use setProgramState)."
       let frp_init = FRPInit current_switch_ref frpstate_ref user_state_ref spawned_threads previous_time_ref t previous_result_ref
       writeIORef current_switch_ref =<< constructSwitch frp_init frpx
       return frp_init

getProgramState :: FRPInit s t i o -> IO s
getProgramState = readIORef . frp_user_state

putProgramState :: FRPInit s t i o -> s -> IO ()
putProgramState frp_init s = writeIORef (frp_user_state frp_init) $ s

modifyProgramState :: FRPInit s t i o -> (s -> s) -> IO ()
modifyProgramState frp_init f = putProgramState frp_init =<< liftM f (getProgramState frp_init)

-- | Bring an FRPProgram up-to-date with the current time or a specific time.
updateFRPProgram :: Maybe Time -> (i,s) -> FRPProgram s i o -> IO (o,s)
updateFRPProgram user_t (i,s) frp_init =
    do actual_t <- getTime
       prev_t <- readIORef $ frp_previous_time frp_init
       when (maybe False (> actual_t) prev_t) $ error "updateFRPProgram: previous time greater than current actual time"
       when (maybe False (> actual_t) user_t) $ error "updateFRPProgram: user time greater than current actual time"
       let t = minimum $ catMaybes [Just actual_t,user_t]
       liftM (fromMaybe $ error "updateFRPProgram: unexpected termination") $ unsafeRunFRPProgram t (i,s) frp_init

frpTest :: (forall e. [FRP e (FRPX () () i o) i o]) -> [i] -> IO [[o]]
frpTest seed_threads inputs =
    do test_program <- newFRPProgram nullaryThreadIdentity $ map (\thread -> ((),thread)) seed_threads
       liftM (map $ map snd . maybe (error "frpTest: unexpected termination") fst) $ 
            mapM (\(t,i) -> unsafeRunFRPProgram t (i,()) test_program) $ zip (map fromSeconds [0.0,0.1..]) inputs

-- | Update an FRPProgram.
unsafeRunFRPProgram :: Time -> (i,s) -> FRPInit s t i o -> IO (Maybe (o,s))
unsafeRunFRPProgram t (i,s) frp_init =
    do prev_t <- readIORef (frp_previous_time frp_init)
       m_o <- flip runContT return $
           do o <- callCC $ \exit ->
                  do let state = FRPState {
                             frpstate_absolute_time = t,
                             frpstate_delta_time = fromMaybe zero $ sub <$> pure t <*> prev_t,
                             frpstate_exit = exit }
                     lift $ writeIORef (frp_state frp_init) state
                     lift $ putProgramState frp_init s
                     action <- lift $ readIORef (frp_current_switch frp_init)
                     action i
              lift $ writeIORef (frp_previous_time frp_init) $ Just t
              lift $ writeIORef (frp_previous_result frp_init) $ o
              return o
       s' <- readIORef (frp_user_state frp_init)
       return $ fmap (\o -> (o,s')) m_o

getFRPState :: FRPInit s t i o -> IO (FRPState i o)
getFRPState = readIORef . frp_state

-- | Shorthand for simple operations in the ContT monad.
frpxOf :: (FRPInit (StateOf m) (ThreadIDOf m) (SwitchInputOf m) (SwitchOutputOf m) ->
              j ->
              ContT (Maybe (SwitchOutputOf m)) IO p) ->
          FRP e m j p
frpxOf action = FRP $ \frpinit -> FactoryArrow $ return $ Kleisli $ action frpinit

-- | Framewise accumulation of signals.
-- The embedded function recieves the current input and the previous output.
accumulate :: p -> (j -> p -> p) -> FRP e m j p
accumulate initial_value accumF = FRP $ \_ -> FactoryArrow $
    do prev_o_ref <- newIORef initial_value
       return $ Kleisli $ \i -> lift $
           do prev_o <- readIORef prev_o_ref
              let o = accumF i prev_o
              writeIORef prev_o_ref o
              _ <- evaluate o
              return o

-- | Get the current absolute time.
absoluteTime :: FRP e m () Time
absoluteTime = frpxOf $ \frpinit () -> lift $ do liftM frpstate_absolute_time $ getFRPState frpinit

-- | Get the change in time since the last update.
deltaTime :: FRP e m () Time
deltaTime = frpxOf $ \frpinit () -> lift $ do liftM frpstate_delta_time $ getFRPState frpinit

-- | Replace the 'frpinit_current_switch' value of the currently running thread with a newly constructed switch.
replaceSwitch :: FRPInit (StateOf m) (ThreadIDOf m) (SwitchInputOf m) (SwitchOutputOf m) -> FRP e m (SwitchInputOf m) (SwitchOutputOf m) -> 
                 ContT (Maybe (SwitchOutputOf m)) IO (SwitchInputOf m -> ContT (Maybe (SwitchOutputOf m)) IO (Maybe (SwitchOutputOf m)))
replaceSwitch frpinit switch =
    do newSwitch <- lift $ constructSwitch frpinit switch
       lift $ writeIORef (frp_current_switch frpinit) newSwitch
       return newSwitch

constructSwitch :: FRPInit (StateOf m) (ThreadIDOf m) (SwitchInputOf m) (SwitchOutputOf m) -> 
                   FRP e m (SwitchInputOf m) (SwitchOutputOf m) -> 
                   IO (SwitchInputOf m -> ContT (Maybe (SwitchOutputOf m)) IO (Maybe (SwitchOutputOf m)))
constructSwitch frp_init (FRP f) =
    do (Kleisli current_switch) <- runFactory $ f frp_init
       return $ \i ->
           do o <- current_switch i
              exit <- liftM frpstate_exit $ lift $ getFRPState frp_init
              exit $ Just o

-- | Whenever a value is provided, change the presently running switch (or thread) to the specified new value,
-- and execute that switch before continuing.  This destroys all state local to the currently running
-- switch (or thread).
-- This function acts as if the switch were performed at frame begin.
switchContinue :: FRP e m (Maybe (FRP e m (SwitchInputOf m) (SwitchOutputOf m)),SwitchInputOf m) (SwitchInputOf m)
switchContinue = frpxOf $ \frpinit (m_switch,i) ->
    do case m_switch of
           (Just switch) ->
               do newSwitch <- replaceSwitch frpinit switch
                  _ <- callCC $ \_ -> newSwitch i
                  error "switchContinue: Unreachable code."
           Nothing -> return i

-- | Whenever a value is provided, change the presently running switch (or thread) to the specified new value,
-- and execute that switch before continuing.  This destroys all state local to the currently running
-- switch (or thread).
-- This function acts as if the switch were performed at frame end.
switchTerminate :: FRP e m (Maybe (FRP e m (SwitchInputOf m) (SwitchOutputOf m)),SwitchOutputOf m) (SwitchOutputOf m)
switchTerminate = frpxOf $ \frp_init (m_switch,o) ->
    do case m_switch of
           (Just switch) ->
               do _ <- replaceSwitch frp_init switch
                  exit <- lift $ liftM frpstate_exit $ getFRPState frp_init
                  _ <- exit $ Just o
                  error "switchTerminate: Unreachable code."
           Nothing -> return o

-- | Spawn new threads once per frame.
spawnThreads :: (ThreadingOf m ~ Enabled) => FRP e m [(ThreadIDOf m,FRP e m (SwitchInputOf m) (SwitchOutputOf m))] ()
spawnThreads = frpxOf $ \frp_init new_threads -> lift $
    do constructed_new_threads <- mapM (uncurry $ unsafeFRPProgram $ frp_spawned_threads frp_init) new_threads
       modifyMVar_ (frp_spawned_threads frp_init) $ return . (constructed_new_threads ++)
       return ()

-- | Kill the current thread, only when the given parameter is true.
killThreadIf :: (ThreadingOf m ~ Enabled) => FRP e m Bool ()
killThreadIf = frpxOf $ \frpinit b ->
    do exit <- lift $ liftM frpstate_exit $ getFRPState frpinit
       when b $ exit Nothing >> return ()
       return ()

-- | Should a thread be allowed to spawn?  Typical values are 'nullaryThreadIdentity', 'forbidDuplicates'.
-- The predicate tests whether or not a particular thread is already running.
type ThreadIdentityRule t = (t -> Bool) -> t -> Bool

-- | Allow unlimited duplicate threads.
nullaryThreadIdentity :: ThreadIdentityRule a
nullaryThreadIdentity _ _ = True

-- | Forbig duplicate threads by equality on the thread identity.
forbidDuplicates :: (Eq t) => ThreadIdentityRule t
forbidDuplicates = (not .)

-- | Allow unlimited duplicate 'Nothing' threads, while restricting all other threads according to the specified rule.
allowAnonymous :: ThreadIdentityRule t -> ThreadIdentityRule (Maybe t)
allowAnonymous _ _ Nothing = True
allowAnonymous r f (Just x) = r (f . Just) x

accumulateThreads :: (Eq t) => ThreadIdentityRule t -> [t] -> [FRPInit s t i o] -> [FRPInit s t i o]
accumulateThreads _ _ [] = []
accumulateThreads rule ts (x:xs) | rule (`elem` ts) (frp_thread_identity x) = x : accumulateThreads rule (frp_thread_identity x : ts) xs
accumulateThreads rule ts (_:xs) | otherwise = accumulateThreads rule ts xs

-- | Get the current thread's identity.
threadIdentity :: FRP e m () (ThreadIDOf m)
threadIdentity = frpxOf $ \frpinit () -> return $ frp_thread_identity frpinit

-- | Construct an arrow from its thread identity.
withThreadIdentity :: (ThreadIDOf m -> FRP e m j p) -> FRP e m j p
withThreadIdentity (actionF) = FRP $ \frp_init ->
    let (FRP actionA) = actionF $ frp_thread_identity frp_init
            in actionA frp_init

data ThreadGroup s t i o = ThreadGroup {
    thread_outputs :: [ThreadResult s t i o],
    thread_group :: MVar [FRPInit s t i o] }

data ThreadResult s t i o = ThreadResult {
    thread_output :: o,
    thread_object :: FRPInit s t i o }

threadResults :: ThreadGroup s t i o -> [(t,o)]
threadResults = map (\t -> (frp_thread_identity $ thread_object t,thread_output t)) . thread_outputs

-- | A complex function that embeds a thread group inside another running thread.  If the parent thread terminates
-- or switches, the embedded thread group is instantly lost.
--
-- 'threadGroup' accepts two paremters:
-- * A transformation from the current state to the nested state.
-- * A state-append function, which takes the original state as the first parameter, and one of the threaded results as the second parameter.
--   This will be run repeatedly to accumulate the output state.
-- * A multithreading algorithm.  The simplest implementation is sequence_.
-- * A list of seed threads with their associated thread identities.
unsafeThreadGroup :: forall e m n.
                     (FRPModel m,FRPModel n,Unwrap n ~ m) => 
                     (StateOf m -> StateOf n) -> 
                     (StateOf m -> StateOf n -> StateOf m) -> 
                     ThreadIdentityRule (ThreadIDOf n) -> 
                     ([IO ()] -> IO ()) -> 
                     [(ThreadIDOf n,FRP e n (SwitchInputOf n) (SwitchOutputOf n))] -> 
                     FRP e m (SwitchInputOf n) (ThreadGroup (StateOf n) (ThreadIDOf n) (SwitchInputOf n) (SwitchOutputOf n))
unsafeThreadGroup sclone sappend rule multithread seed_threads = FRP $ \frp_init -> FactoryArrow $
    do threads <- newEmptyMVar
       putMVar threads =<< (mapM (uncurry $ unsafeFRPProgram threads) seed_threads)
       let runThreads :: [ThreadIDOf n] -> SwitchInputOf n -> IO [ThreadResult (StateOf n) (ThreadIDOf n) (SwitchInputOf n) (SwitchOutputOf n)]
           runThreads already_running_threads j =
               do threads_this_pass <- liftM (accumulateThreads rule already_running_threads) $ takeMVar threads
                  putMVar threads []
                  s_orig_clone <- liftM sclone $ getProgramState frp_init
                  absolute_time <- liftM frpstate_absolute_time $ getFRPState frp_init
                  multithread $ map (\t -> unsafeRunFRPProgram absolute_time (j,s_orig_clone) t >> return ()) threads_this_pass
                  results_this_pass <- liftM catMaybes $ forM threads_this_pass $ \t ->
                      do m_o <- readIORef (frp_previous_result t)
                         s <- readIORef (frp_user_state t)
                         modifyProgramState frp_init (`sappend` s)
                         return $
                             do o <- m_o
                                return $ ThreadResult o t
                  results <- liftM (results_this_pass++) (if null threads_this_pass 
                                                          then return [] 
                                                          else runThreads (nub $ map frp_thread_identity threads_this_pass ++ already_running_threads) j)
                  modifyMVar_ threads (return . ((map thread_object results_this_pass)++))
                  return results
       return $ Kleisli $ \j ->
           do results <- lift $ runThreads [] j
              return $ ThreadGroup {
                  thread_outputs = results,
                  thread_group = threads }

-- | Embed some threads inside another running thread, as 'threadGroup'.
frpContext :: (RecombinantState s,s ~ StateOf m,FRPModel m,Eq t) => 
              ThreadIdentityRule t -> [(t,FRP e (FRPContext t j p m) j p)] -> FRP e m j [(t,p)]
frpContext rule seed_threads = arr threadResults . unsafeThreadGroup clone recombine rule sequence_ seed_threads

-- | Embed a single-threaded, bracketed switch inside another running thread.
frp1Context :: (FRPModel m) => FRP e (FRP1Context j p m) j p -> FRP e m j p
frp1Context thread = proc i ->
    do os <- withThreadIdentity (\t -> unsafeThreadGroup id (const id) nullaryThreadIdentity sequence_ [(t,thread)]) -< i
       returnA -< case threadResults os of
           [(_,o)] -> o
           _ -> error "frp1Context: unexpected non-singular result."

-- | Run a computation only when the input is defined.
whenJust :: (FRPModel m) => (forall x y. FRP e (FRP1Context x y m) j p) -> FRP e m (Maybe j) (Maybe p)
whenJust actionA = frp1Context whenJust_
    where whenJust_ = proc i ->
              do switchContinue -< (maybe (Just whenNothing_) (const Nothing) i,i)
                 arr (Just) <<< actionA -< fromMaybe (error "whenJust: impossible case") i
          whenNothing_ = proc i ->
              do switchContinue -< (fmap (const whenJust_) i,i)
                 returnA -< Nothing

-- | Perform an IO action when a stream is first initialized.
ioInit :: (InputOutputOf m ~ Enabled) => (IO p) -> FRP e m () p
ioInit action = FRP $ \_ -> FactoryArrow $
    do p <- action
       return $ Kleisli $ const $ return p

-- | Perform an arbitrary IO action.
ioAction :: (InputOutputOf m ~ Enabled) => (j -> IO p) -> FRP e m j p
ioAction action = frpxOf $ \_ j -> lift $ action j

-- | Send tagged information.
outgoingBy :: (j -> j -> Bool) ->
              -- ^ Equality predicate as described in 'newTransmitterBy'.
              FRP e m j (Message j)
outgoingBy f = FRP $ \_ -> FactoryArrow $
    do t <- newTransmitterBy f
       return $ Kleisli $ lift . transmit t

-- | Send tagged information.
outgoing :: (Eq j) => FRP e m j (Message j)
outgoing = outgoingBy (==)

-- | Receive tagged information, with memoization.
incoming :: FRP e m (Message j) j
incoming = FRP $ \_ -> FactoryArrow $
    do r <- newReceiver
       return $ Kleisli $ lift . receive r

-- | Get a bounded random value, as 'randomRIO'.  A new value is pulled for each
-- frame of animation.
randomA :: (Random a) => FRP e m (a,a) a
randomA = frpxOf $ \_ ->
    lift . randomRIO

