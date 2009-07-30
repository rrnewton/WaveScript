{-# LANGUAGE FlexibleInstances, BangPatterns, MagicHash #-}

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

--module Cnc3 where 

import Data.Set as Set
import Data.HashTable as HT
import Data.Int
import Data.IORef
import Control.Monad
import Control.Concurrent.MVar
import System

import System.IO.Unsafe

--import Control.Concurrent
import GHC.Conc

import GHC.Prim

-- GRRR
import GHC.Exts 

--par a b = b

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

call :: Ord a  => TagCol a -> a          -> IO ()
put  :: Show a => ItemCol a b -> a -> b  -> IO ()
get  ::           ItemCol a b -> a       -> IO b

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

-- Here's where we spawn new work.
call (_set,_steps) tag = 
    do set <- readIORef _set
       steps <- readIORef _steps
       if Set.member tag set
	then do writeIORef _set (Set.insert tag set)
		return ()
	else foldM (\ () step -> 
		    do let v = step tag
--		       unsafePerformIO v `par` v)  -- Nope..
		       unsafePerformIO v `par` return ()) -- Haha.. finishes too soon.  Need to wait.
--		       unsafePerformIO v `par` unsafeInterleaveIO v)
		 () steps
			 
-- If it's not there we add the mvar ourselves then block:
-- FIXME: what we need is a concurrent hash table here so that we can 
-- FIXME: we also need an I-var!!
put col tag item = 
    do mvar <- assureMvar col tag 
       bool <- tryPutMVar mvar item
       if not bool then error ("Already an item with tag " ++ show tag) else return ()
	       
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


----------------------------------------
-- Primes example:

-- Wow this is the SAME, performance wise. 
-- GHC is already getting all the unboxing:
{-
isPrime :: Int -> Bool
isPrime 2 = True
isPrime !n = (prmlp 3# ==# n#)
    where (I# n#) = n
	  prmlp :: Int# -> Int#
	  prmlp !i = if (remInt# n# i) ==# 0#
		     then i else prmlp (i +# 2#)
-}

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = (prmlp 3 == n)
    where prmlp :: Int -> Int
  	  prmlp i = if (rem n i) == 0
 		    then i else prmlp (i + 2)


primes n = 
   do primes <- newItemCol() :: IO (ItemCol Int Int)
      put primes 2 2
      tags <- newTagCol()
      prescribe tags (\t -> if isPrime (t) 
		            then put primes t t
		            else return ())
      let loop i | i >= n = return ()
  	  loop i = do call tags i 
	              loop (i+2)
      loop 3
      result <- itemsToList primes
      --return (take 30 (Prelude.map fst result))
      return (length result)	       

mymain = do [n] <- System.getArgs 
	    x <- primes ((read n)::Int)
	    putStrLn (show x)
main = mymain

-- Test the serial function:
serial n = serlp 3 1
   where serlp :: Int -> Int -> Int
	 serlp i c | i >= n    = c
  	 serlp i c | isPrime i = serlp (i+2) (c+1)
	 serlp i c             = serlp (i+2) c

-- main = do [n] <- System.getArgs 
-- 	  putStrLn "Running serial version of primes..."
-- 	  putStrLn $ show $ serial ((read n)::Int)

-- For reference, here's a sieve :
primels :: [Integer]
primels = 2 : Prelude.filter isPrime [3,5..]
     where
     isPrime n   = all (not . divides n) $ takeWhile (\p -> p*p <= n) primels
     divides n p = n `mod` p == 0

--main = putStrLn (show (length (take 9592 primels)))
--main = putStrLn (show (take 50 primels))

-- Alas this is 3X slower than the C version to start with.

