{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures #-}

#include "Cnc3.hs"
-- #include "Cnc2_wmagic.hs"

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

main = do [n] <- System.getArgs 
	  x <- runGraph $ primes ((read n)::Int)
	  putStrLn (show x)
