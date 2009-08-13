{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , PatternSignatures
  #-}

{-
  This is the third version.

  I believe that we need to use unsafePerformIO to communicate to GHC
  that the whole execution really is pure.  

  This version formulates steps as side-effecting functions on hash tables of MVars.

  If we had concurrent hashtables, then we could use par annotations
  to parallelize a tree of tasks(and treat tag-puts as function
  calls), or we could use forkIO to introduce a lightweight thread for
  each tag+step pair.

  Presently, I just pretend that hashtables are concurrent.  This file
  is a "what-if" scenario.
 -}


#ifndef INCLUDEMETHOD
module Cnc where
#else
#warning "Loading imperative, IO-based CnC implementation."
#endif

import Data.Set as Set
import Data.HashTable as HT
import Data.Map as Map
import Data.Int
import Data.IORef
import Data.Complex
import Data.Word
import Control.Monad
import Control.Concurrent.MVar
import System
import System.IO.Unsafe

import Control.Concurrent
import GHC.Conc
import GHC.Prim

-- GRRR
import GHC.Exts 

import Util

--par a b = b

------------------------------------------------------------
-- Configuration Toggles:

#ifdef MEMOIZE
#warning "Memoization enabled"
memoize = True
#else
memoize = False
#endif


------------------------------------------------------------
-- Abstract over the shared mutable data structure used for item
-- collections:

#ifdef HASHTABLE_TEST
type MutableMap a b = HashTable a (MVar b)
newMutableMap :: (Eq tag, Hashable tag) => IO (MutableMap tag b)
newMutableMap = HT.new (==) hash
assureMvar col tag = 
  do mayb <- HT.lookup col tag
     case mayb of 
         Nothing -> do mvar <- newEmptyMVar
		       HT.insert col tag mvar
		       return mvar
	 Just mvar -> return mvar
mmToList = HT.toList
#warning "Enabling HashTable item collections.  These are not truly thread safe (yet)."
#else
-- A Data.Map based version:
type MutableMap a b = IORef (Map a (MVar b))
newMutableMap :: (Ord tag) => IO (MutableMap tag b)
newMutableMap = newIORef Map.empty
assureMvar col tag = 
  do map <- readIORef col
     case Map.lookup tag map of 
         Nothing -> do mvar <- newEmptyMVar
		       atomicModifyIORef col 
			  (\mp -> 
			   let altered = Map.alter 
			                  (\mv -> 
					    case mv of
					     Nothing -> Just mvar
					     Just mv -> Just mv)
			                  tag mp 
			   -- Might be able to optimize this somehow...
			   in (altered, altered!tag))
	 Just mvar -> return mvar
mmToList col = 
    do map <- readIORef col 
       return (Map.toList map)
#endif

------------------------------------------------------------
-- Type definitions:

-- FIXME: Switch to MVars for thread safety:
type TagCol a    = (IORef (Set a), IORef [Step a])
type ItemCol a b = MutableMap a b
type Step a = a -> IO ()

------------------------------------------------------------
-- (Optional) type signatures for operations:

--newWorld = return () 

-- Basically just a table for memoization:
newTagCol  :: () -> IO (TagCol tag)
newItemCol :: (Eq tag, Ord tag, Hashable tag) => () -> IO (ItemCol tag b)
prescribe  :: TagCol tag -> Step tag -> IO ()

call :: Ord a  => TagCol a -> a          -> IO ()
--call :: TagCol a -> a -> IO ()
put  :: (Show a, Ord a) => ItemCol a b -> a -> b  -> IO ()
get  :: Ord a           => ItemCol a b -> a       -> IO b

initialize :: CncCode a -> GraphCode a
finalize   :: CncCode a -> GraphCode a

class Hashable a where
    hash :: a -> Int32

instance Hashable Int where
    hash = hashInt
instance Hashable Char where
    hash = hashInt . fromEnum 
instance Hashable Word16 where
    hash = hashInt . fromIntegral
--instance Hashable String where -- Needs -XTypeSynonymInstances 
instance Hashable [Char] where
    hash = hashString
instance (Hashable a, Hashable b) => Hashable (a,b) where 
    hash (a,b) = hash a + hash b

-- Needs -fallow-undecidable-instances:
-- instance Integral t => Hashable t where
--     hash n = hashInt (fromInteger (toInteger n))
-- instance Enum a => Hashable a where
--     hash = hashInt . fromEnum 


--------------------------------------------------------------------------------
-- Implementation:

-- Need an argument if we don't want to run in to the monomorphism
-- restriction (-fno-monomorphism-restriction)
newItemCol () = newMutableMap
newTagCol () = do ref1 <- newIORef Set.empty
		  ref2 <- newIORef []
		  return (ref1, ref2)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- FIXME: use a trampoline here..
--  I think this is our stack space leak:

--call :: Ord a  => TagCol a -> a          -> IO ()
-- proto_call :: (Ord a1) =>
-- 	      (a ->  -> IO ()) -> 
-- 	       TagCol a -> a -> IO ()
proto_call action tc@(_set,_steps) tag = 
    do set   <- readIORef _set
       steps <- readIORef _steps
       if memoize 
        then if Set.member tag set
	     then return ()
	     else do writeIORef _set (Set.insert tag set)
		     action steps tag
        else action steps tag


call1 :: Ord a  => TagCol a -> a -> IO ()
call2 :: Ord a  => TagCol a -> a -> IO ()
call3 :: Ord a  => TagCol a -> a -> IO ()
call4 :: Ord a  => TagCol a -> a -> IO ()
call5 :: Ord a  => TagCol a -> a -> IO ()
call6 :: Ord a  => TagCol a -> a -> IO ()

finalize3 :: CncCode a -> GraphCode a
finalize4 :: CncCode a -> GraphCode a
finalize5 :: CncCode a -> GraphCode a
finalize6 :: CncCode a -> GraphCode a

------------------------------------------------------------
--Version 1: Serial
-- Call is where we spawn new work.
call1 = proto_call (\ steps tag -> foldM (\ () step -> step tag) () steps)

-- Version 2: 
-- Here we do the tail call optimization for the common case of a single prescribed step.
call2 = proto_call (\ steps tag -> 
		    case steps of 
		     [step] -> step tag
		     steps -> foldM (\ () step -> step tag) () steps)

------------------------------------------------------------
-- Version 3: Here we try for forked parallelism:

call3 = proto_call (\ steps tag -> 
		    case steps of 
	            -- Enable the tail-call optimization even with the parallel scheduler!!
		    --	         [step] -> step tag
	             steps -> 
		      foldM (\ () step -> do forkIO (step tag); return ())
   	  	       () steps)

get3 col tag = do mvar <- assureMvar col tag 
		  readMVar mvar

-- The above 'call's use a trivial finalizer:
-- WARNING -- this will not wait for workers to finish during finalization.
-- Therefore, this only works with programs that 'get' their output.
finalize3 x = x 
-- TODO: At least kil off the existing threads here?

------------------------------------------------------------
-- Version 4: A global work queue.
-- INCORRECT: This version just lets threads die when they block!

-- This version uses a global work-queue.
-- Here laziness comes in handy, we queue the thunks.
call4 = proto_call (\ steps tag -> 
		    foldM (\ () step -> writeChan global_queue (step tag))
		      () steps)

-- Then at finalize time we set up the workers and run them.
finalize4 finalAction = 
    do joiner <- newChan 
       let worker = do e <- isEmptyChan global_queue
		       if e then writeChan joiner ()
			    else do action <- readChan global_queue 
				    action 
				    worker 
       -- Fork one worker per thread:
       mapM (\n -> forkOnIO n worker) [0..numCapabilities-1]
       mapM_ (\_ -> readChan joiner)  [0..numCapabilities-1]
       finalAction

get4 :: Ord a => ItemCol a b -> a -> IO b
get4 = get3

-- TODO: we should do a better job here of using a monad transformer on top of IO:
-- But if we must keep the same CnC interface... This is expedient:
global_queue :: Chan (IO ())
global_queue = unsafePerformIO newChan

------------------------------------------------------------
-- Version 5: Blocking with replacement.

call5 = call4

-- Then at finalize time we set up the workers and run them.
finalize5 finalAction = 
    do joiner <- newChan 
       let worker = 
	       do e <- isEmptyChan global_queue
		  if e then writeChan joiner ()
		       else do action <- readChan global_queue 
			       action 
			       myId <- myThreadId
			       set <- readIORef global_mortalthreads
			       if Set.notMember myId set
				  then worker
				  else writeChan joiner ()
       writeIORef global_makeworker (worker)
       atomicModifyIORef global_numworkers (\n -> (n + numCapabilities, ()))
       -- Fork one worker per thread:
--       mapM (\n -> forkOnIO n (worker)) [0..numCapabilities-1]
       putStrLn$ "Forking "++ show numCapabilities ++" threads"
       mapM (\n -> forkIO (worker)) [0..numCapabilities-1]
 --      mapM_ (\_ -> readChan joiner)  [0..numCapabilities-1]
       let waitloop = do num <- readIORef global_numworkers
			 putStrLn ("=== Waiting on "++ show num)
	                 if num == 0
			  then return () 
			  else do readChan joiner
				  atomicDecr global_numworkers
				  waitloop
       waitloop
       finalAction

-- If we block our own thread, we need to issue a replacement.
get5 col tag = 
    do mvar <- assureMvar col tag 
       hopeful <- tryTakeMVar mvar
       case hopeful of 
         Just v -> return v
         Nothing -> do action <- readIORef global_makeworker
		       atomicIncr global_numworkers
		       -- If this were CPS then we would just give our
		       -- continuation to the forked thread.  Alas, no.
		       myId  <- myThreadId
		       forkIO action
		       -- The safe way:
		       atomicModifyIORef global_mortalthreads (\s -> (Set.insert myId s, ()))
		       putStrLn " >>> Blocked ||| "
		       readMVar mvar

-- This is a bit silly, this emulates "thread local storage" to let
-- each worker thread know whether it is recursive (True) or "oneshot".
global_mortalthreads :: IORef (Set ThreadId)
global_mortalthreads = unsafePerformIO (newIORef Set.empty)

global_numworkers :: IORef Int
global_numworkers = unsafePerformIO (newIORef 0)

global_makeworker :: IORef (IO ())
global_makeworker = unsafePerformIO$ newIORef (return ())

------------------------------------------------------------
-- Version 6: Thread spammer -- fork a permanent worker every time we block.

call6 = call5

-- Then at finalize time we set up the workers and run them.
finalize6 finalAction = 
    do joiner <- newChan 
       let worker = 
	       do e <- isEmptyChan global_queue
		  if e then writeChan joiner ()
		       else do action <- readChan global_queue 
			       action 
			       worker
       writeIORef global_makeworker (worker)
       atomicModifyIORef global_numworkers (\n -> (n + numCapabilities, ()))
       -- Fork one worker per thread:
       putStrLn$ "Forking "++ show numCapabilities ++" threads"
       mapM (\n -> forkIO (worker)) [0..numCapabilities-1]
       let waitloop = do num <- readIORef global_numworkers
			 putStrLn ("=== Waiting on "++ show num)
	                 if num == 0
			  then return () 
			  else do readChan joiner
				  atomicDecr global_numworkers
				  waitloop
       waitloop
       finalAction

-- If we block our own thread, we need to issue a replacement.
get6 col tag = 
    do mvar <- assureMvar col tag 
       hopeful <- tryTakeMVar mvar
       case hopeful of 
         Just v -> return v
         Nothing -> do action <- readIORef global_makeworker
		       atomicIncr global_numworkers
		       forkIO action
		       putStrLn " >>> Blocked ||| "
		       readMVar mvar


------------------------------------------------------------

-- Pick an implementation:
#if CNC_SCHEDULER == 3
get = get3 ; call = call3 ; finalize = finalize3
#elif CNC_SCHEDULER == 4
get = get4 ; call = call4 ; finalize = finalize4
#elif CNC_SCHEDULER == 5
get = get5 ; call = call5 ; finalize = finalize5
#elif CNC_SCHEDULER == 6
get = get6 ; call = call6 ; finalize = finalize6
#else
#error "Cnc.hs -- CNC_SCHEDULER is not set to one of {3,4,5,6}"
#endif

-- get      =      get5
-- call     =     call5 
-- finalize = finalize5



-- If it's not there we add the mvar ourselves then block:
-- FIXME: what we need is a concurrent hash table here ...
-- FIXME: we also need an I-var!!
put col tag item = 
    do mvar <- assureMvar col tag 
       bool <- tryPutMVar mvar item
#ifdef REPEAT_PUT_ALLOWED
       return ()
#else
       if not bool then error ("Already an item with tag " ++ show tag) else return ()
#endif

-- A tag collection stores the list of subscribed step collections.
-- Simply add it to the list:
prescribe (_set,_steps) step = 
    do steps <- readIORef _steps
       writeIORef _steps (step:steps)


--------------------------------------------------------------------------------
-- Common interface for interoperating with my alternate implementations:

-- For this implementation we don't have separate monads for CncCode and GraphCode.

type CncCode   a = IO a
type GraphCode a = IO a

-- Embed CncCode in the graph construction program:
initialize x = x

-- Bring us from the graph monad back to the IO monad:
runGraph :: CncCode a -> IO a
runGraph x = x

--------------------------------------------------------------------------------

atomicIncr x = atomicModifyIORef x (\n -> (n+1, ()))
atomicDecr x = atomicModifyIORef x (\n -> (n-1, ()))

-- --------------------------------------------------------------------------------
-- -- Test program:

itemsToList :: ItemCol a b -> IO [(a,b)]
itemsToList ht = 
 do ls <- (mmToList ht)
    foldM (\ acc (key,mvar) -> 
	   do val <- readMVar mvar
	      return $ (key,val) : acc)
	  [] ls

incrStep d1 (t2,d2) tag = 
 do val <- get d1 tag 
    putStr ("  ("++ show tag ++") Incrementing " ++ show val ++"\n")
    put d2 tag (val + 1)
    call t2 tag

test = -- Allocate collections:
    do t1 <- newTagCol()
       t2 <- newTagCol()
       t3 <- newTagCol()
       d1 <- newItemCol()
       d2 <- newItemCol()
       d3 <- newItemCol()
        -- Initialize:
       put d1 'a' 33
       put d1 'b' 100
       -- Build and execute the graph:
       prescribe t1 (incrStep d1 (t2,d2))
       prescribe t2 (incrStep d2 (t3,d3))
       -- Start things up:	 
       call t1 'a'
       call t1 'b'
       -- Read the results (waits until they're ready):
       result1 <- get d3 'a'
       result2 <- get d3 'b'
       return (result1, result2) 

