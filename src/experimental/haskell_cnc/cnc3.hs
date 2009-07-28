{-# LANGUAGE FlexibleInstances   #-}
{-
  This is the third version.

  I believe that we need to use unsafePerformIO to communicate to GHC
  that the whole execution really is pure.  

  This version makes steps into side-effecting functions on hash tables.

  If we had concurrent hashtables, then we could use par annotations
  to parallelize a tree of tasks(and treat tag-puts as function
  calls), or we could use forkIO to introduce a lightweight thread for
  each tag+step pair.

 -}

import Data.Set as Set
import Data.Map as Map
import Data.HashTable as HT
import Data.Int

import Data.IORef

import Control.Monad
import Control.Concurrent.MVar
import Control.Parallel
--import Control.Parallel.Strategies

import System.IO.Unsafe

------------------------------------------------------------
-- Type definitions:

--type TagCol a    = IORef (Set a)
--type TagCol a    = HashTable a [Step a]
type TagCol a    = (IORef (Set a), IORef [Step a])
type ItemCol a b = HashTable a (MVar b)
type Step a = a -> IO ()


-- The monad stores all the information in the tag/item collections
-- It's a state thread.  There only needs to be ONE state thread currently.
--type CncCode s a = ST s a

-- A graph data structure keeps track of the steps that are prescribed
-- everything that is needed by the scheduler.
-- Do we need a different monad for wiring together the graph??
-- Not in this version...
--data Graph s = Graph (STRef s (Map TagColID [MatchedPair s]))

--data Graph s = Graph (STRef s (Map TagColID [MatchedPair s]))


-- Tricky existential types:
-- Here's a tag collection and associated step that "match" tag-types:
--data MatchedPair s = forall t. Ord t => MP (TagCol s t, Step s t)


------------------------------------------------------------
-- (Optional) type signatures for operations:

-- Basically just a table for memoization:
newTagCol  :: () -> IO (TagCol tag)
--newTagCol  :: (Eq tag, Hashable tag) => () -> IO (TagCol tag)
newItemCol :: (Eq tag, Hashable tag) => () -> IO (ItemCol tag b)

call :: Ord a => TagCol a -> a          -> IO ()
put  ::          ItemCol a b -> a -> b  -> IO ()
get  ::          ItemCol a b -> a       -> IO b

-- newGraph  :: CncCode s (Graph s)
--prescribe :: TagCol tag -> Step tag -> IO ()

class Hashable a where
    hash :: a -> Int32

instance Hashable Int where
    hash = hashInt

-- Needs -fallow-undecidable-instances:
--instance Integral t => Hashable t where
--    hash n = hashInt (fromInteger (toInteger n))

--instance Hashable String where -- Needs -XTypeSynonymInstances 
instance Hashable [Char] where
    hash = hashString

--------------------------------------------------------------------------------
-- Implementation:

-- Need an argument if we don't want to run in to the monomorphism
-- restriction (-fno-monomorphism-restriction)
newItemCol () = HT.new (==) hash
newTagCol () = do ref1 <- newIORef Set.empty
		  ref2 <- newIORef []
		  return (ref1, ref2)
--newTagCol () = HT.new (==) hash

call (_set,_steps) tag = 
--    let v = step tag in v `par` v	 
    do set <- readIORef _set
       steps <- readIORef _steps
       if Set.member tag set
	then do writeIORef _set (Set.insert tag set)
		return ()
	else foldM (\ () step -> 
		    do let v = step tag
		       v `par` v)
		 () steps
			 
--do let v = step tag

-- If it's not there we add the mvar ourselves then block:
-- FIXME: what we need is a concurrent hash table here so that we can 
-- FIXME: we also need an I-var!!
put col tag item = 
    do mvar <- assureMvar col tag 
       putMVar mvar item

get col tag = 
    do mvar <- assureMvar col tag 
       readMVar mvar

assureMvar col tag = 
  do mayb <- HT.lookup col tag
     case mayb of 
         Nothing -> do mvar <- newEmptyMVar
		       HT.insert col tag mvar
		       return mvar
	 Just mvar -> return mvar


-- newGraph = do ref <- newSTRef (Map.empty :: Map TagColID [MatchedPair s])
-- 	      return (Graph ref)

-- prescribe (Graph graph) (MP (tagc,step)) = 
--     do stepmap <- readSTRef graph
--        (id,frnt,bck) <- readSTRef tagc
--        let ls = Map.findWithDefault ([]::[MatchedPair s]) id stepmap 
--        let pr = MP (tagc,step) 
--        writeSTRef graph (Map.insert id (pr:ls) stepmap)
--        return ()

-- runSimpleScheduler (Graph graph) = 
--     do    stepmap <- readSTRef graph
-- 	  unsafeIOToST $ putStr ("Size of map " ++ show (Map.size stepmap) ++"\n")
-- 	  allempty <-
-- 	   Map.fold (\ ls acc ->   -- For each tag collection:
-- 		    case head ls of
-- 		     (MP (tags,_)) -> 
-- 		        do (id,frnt,bck) <- readSTRef tags
-- 		           bool <- acc
-- 		           let newbool = bool && Set.null frnt
-- 		           -- For each step prescribed by this tag collection:
-- 		           foldr (\ (MP (tags,step)) (acc) ->
-- 				  do (id,frnt,bck) <- readSTRef tags
-- 				     -- Invoke the step:
-- 				     Set.fold (\ tag acc -> do acc; step tag)
-- 				              acc frnt)
-- 		                 (return ()) ls
-- 		           -- After we're done consuming the fresh tags, clear them:
-- 		           writeSTRef tags (id, Set.empty, Set.union frnt bck)
-- 		           unsafeIOToST $ putStr ("Finished tag collection " ++ show id ++"\n")
-- 		           return newbool)
-- 		   (return True)
--          	   stepmap	  
-- 	  if allempty
-- 	   then unsafeIOToST $ putStr ("All tag collection inboxes empty.  Finished.\n")
-- 	   else runSimpleScheduler (Graph graph)


-- --------------------------------------------------------------------------------
-- -- Test program:

-- incrStep d1 (t2,d2) tag = 
--  do val <- get d1 tag 
--     unsafeIOToST $ putStr ("  ("++ show tag ++") Incrementing " ++ show val ++"\n")
--     put d2 tag (val + 1)
--     call t2 tag

test = -- Allocate collections:
    do t1 <- newTagCol()
       t2 <- newTagCol()
       t3 <- newTagCol()
--       d1 <- newItemCol()
--       d2 <- newItemCol()
--       d3 <- newItemCol()

--        -- Initialize:
--        put d1 'a' 33
--        put d1 'b' 100
--       call t1 'a'
--        call t1 'b'

       set <- readIORef (fst t1)
       steps <- readIORef (snd t1)
       return set 

--        -- Build and execute the graph:
--        graph <- newGraph
--        prescribe graph (MP (t1, incrStep d1 (t2,d2)))		 
--        prescribe graph (MP (t2, incrStep d2 (t3,d3)))
--        runSimpleScheduler graph
--        result1 <- get d3 'a'
--        result2 <- get d3 'b'
--        return (result1, result2) 


-- go = runST test


{- 

ghci -XRankNTypes cnc.hs
ghci -fglasgow-exts cnc.hs

-}

--instance Monad CncCode where
--    Hmm [a] >>= f  =  f a
--    return x = Hmm [x]

