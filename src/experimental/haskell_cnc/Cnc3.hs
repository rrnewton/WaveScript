{-# LANGUAGE FlexibleInstances   #-}
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

module Cnc3 where 

import Data.Set as Set
import Data.HashTable as HT
import Data.Int
import Data.IORef
import Control.Monad
import Control.Concurrent.MVar
import Control.Parallel

------------------------------------------------------------
-- Type definitions:

type TagCol a    = (IORef (Set a), IORef [Step a])
type ItemCol a b = HashTable a (MVar b)
type Step a = a -> IO ()

------------------------------------------------------------
-- (Optional) type signatures for operations:

-- Basically just a table for memoization:
newTagCol  :: () -> IO (TagCol tag)
newItemCol :: (Eq tag, Hashable tag) => () -> IO (ItemCol tag b)
prescribe  :: TagCol tag -> Step tag -> IO ()

call :: Ord a => TagCol a -> a          -> IO ()
put  ::          ItemCol a b -> a -> b  -> IO ()
get  ::          ItemCol a b -> a       -> IO b

class Hashable a where
    hash :: a -> Int32

instance Hashable Int where
    hash = hashInt
instance Hashable Char where
    hash = hashInt . fromEnum 
--instance Hashable String where -- Needs -XTypeSynonymInstances 
instance Hashable [Char] where
    hash = hashString

-- Needs -fallow-undecidable-instances:
{-
instance Integral t => Hashable t where
    hash n = hashInt (fromInteger (toInteger n))
instance Enum a => Hashable a where
    hash = hashInt . fromEnum 
-}

--------------------------------------------------------------------------------
-- Implementation:

-- Need an argument if we don't want to run in to the monomorphism
-- restriction (-fno-monomorphism-restriction)
newItemCol () = HT.new (==) hash
newTagCol () = do ref1 <- newIORef Set.empty
		  ref2 <- newIORef []
		  return (ref1, ref2)
call (_set,_steps) tag = 
    do set <- readIORef _set
       steps <- readIORef _steps
       if Set.member tag set
	then do writeIORef _set (Set.insert tag set)
		return ()
	else foldM (\ () step -> 
		    do let v = step tag
		       v `par` v)
		 () steps
			 
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

-- Simply add it to the list:
prescribe (_set,_steps) step = 
    do steps <- readIORef _steps
       writeIORef _steps (step:steps)

-- --------------------------------------------------------------------------------
-- -- Test program:

itemsToList :: ItemCol a b -> IO [(a,b)]
itemsToList ht = 
 do ls <- (HT.toList ht)
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

--        set <- readIORef (fst t1)
--        steps <- readIORef (snd t1)
--        ls <- itemsToList d1
--        ls2 <- itemsToList d2
--        return ls2
