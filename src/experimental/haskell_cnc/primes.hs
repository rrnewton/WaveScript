{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures #-}

-- #define MEMOIZE
#include "stub.h"

----------------------------------------
-- Primes example:

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = (prmlp 3 == n)
    where prmlp :: Int -> Int
  	  prmlp i = if (rem n i) == 0
 		    then i else prmlp (i + 2)

-- Wow manually unboxed version is the SAME, performance wise. 
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

primes n = 
   do primes :: ItemCol Int Int <- newItemCol()       
      tags <- newTagCol()
      prescribe tags (\t -> if isPrime (t) 
		            then put primes t t
		            else return ())

      let loop i | i >= n = return ()
  	  loop i = do call tags i 
	              loop (i+2)
      initialize $
	do put primes 2 2
           loop 3
      finalize $ 
        do result <- itemsToList primes
	   --return (take 30 (Prelude.map fst result))
	   return (length result)	       
	   --return result

--primes n = return $ serial n

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

main = do args <- System.getArgs 
	  case args of 
	   [] -> error "primes takes an argument!"
	   [n] -> 
	     do x <- runGraph $ primes ((read n)::Int)
		putStrLn (show x)
{- 
NOTES:

[2009.08.12]
 * Primes 100K:
    - pure: 2.543
    - io:   2.544
   IO uses less memory, however.

 * Primes 200K:
    - pure: 9.47
    - io:   9.42

This is what we would expect, it's absolutely dominated by the kernel.
Surprisingly, disabling memoization doesn't change memory usage perciptibly.

---------
 Trying GHC 6.10.4:
  (same)

---------
 Trying for parallelism.
 Using the forkIO-on-every-call version:
 With the tail-call optimization in Cnc.hs I get 9 threads but 100% cpu usage.
 When I disable that, I start seeing 2-300% cpu usage.
   For 200K:
     - nothreads : 10.04 real
     - w/threads : 5.35/9.6 real/user  | -N2
     - w/threads : 4.04 real (9.8 user)  -N4
     - w/threads : 3.36/10.3 real/user | -N5
     - w/threads : 4.15/10.0 real/user | -N6
     - w/threads : 3.8/11.6 real/user  | -N7
     - w/threads : 8.2/23.4 real/user  | -N8

And with some other options:

     - w/threads : 9.7 real  | -N5 -qm
     - w/threads : 9.9 real/user | -N5 -qm -qw
     - w/threads : 3.33 real/user | -N5 -H1G

 Increasing context switch time with -C150ms doesn't help either.

 GC is 8.3%, not bad.

--------
  Ok, so why is the tail call optimization wrong?

  Well, that's pretty clear.  Many apps only do a single 'call' from
  each step, but they do many calls initially.  Problem is, many calls
  from one "CncCode" doesn't spawn extra threads... only a single call
  that has more than one downstream spawns multiple threads.

  The optimization is definitely broken.  How to fix it?
  Problem is that with the current IO based formulation calls happen immediately,
  we don't have any context to know whether there was more than one... 

  Could introduce an explicit "tailcall" form.

-------- 
  A different strategy is to just switch gears and look at other
  schedulers.  I did a global-work-queue scheduler.  

  In my first version, without -threaded I get 10.04 seconds real.
  Not a noticable slowdown for the thread mortality check on each
  action...  (And it's the same whether thread mortality is tracked in
  a HashTable or an IORef Map.)

  Uh oh, it gets reasonably good speedup when I enable multiple threads.
  BUT it sometimes gets threads that are blocked indefinitely.
  Odd... on this quadcore it only happens on primes with -N5 (not with -N3).
  And only sometimes with -N5... Wow, with this scheduler -N5 has horrible performance.

  It could be the underlying unsafety of the non-concurrent hashtables I'm using.
  After all, I know this to be a broken implementation.

  Well darn, now I'm getting "thread blocked indefinitely" with
  scheduler 5 even with using Data.Map based item collections!
  By the way, with sched 5 & Maps, I get 3.69 seconds realtime.
  We would expect maps not to matter for primes.

-}
