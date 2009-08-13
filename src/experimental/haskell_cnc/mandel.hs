{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures #-}

-- #define MEMOIZE
#include "stub.h"

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 0
  where   
   fn = magnitude
   loop i z count
    | i == max_depth = count
    | fn(z) >= 2.0   = count 
    | otherwise      = loop (i+1) (z*z + c) (count+1)

type Pair = (Word16, Word16)

mandelProg :: Int -> Int -> Int -> GraphCode Int
mandelProg max_row max_col max_depth = 
    do position :: TagCol  Pair                  <- newTagCol()
       dat      :: ItemCol Pair (Complex Double) <- newItemCol()
       pixel    :: ItemCol Pair Int              <- newItemCol()   
       
       let mandelStep tag = 
	    do cplx <- get dat tag
	       put pixel tag (mandel max_depth cplx)

       prescribe position mandelStep 

--        gcPrintWorld "1"
       initialize $ 
        for_ 0 max_row $ \i -> 
         for_ 0 max_col $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do put dat (_i,_j) z
	     call position (_i,_j)
--        gcPrintWorld "2"

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ 
	foldRange 0 max_row (return 0) $ \acc i -> 
	 foldRange 0 max_col acc $ \acc j -> 
	   do cnt <- acc
	      p <- get pixel (fromIntegral i, fromIntegral j)
	      if p == max_depth
   	       then return (cnt + (i*max_col + j))
   	       else return cnt
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


runMandel = do (a:b:c:_) <- System.getArgs 
	       check <- runGraph $ mandelProg (read a) (read b) (read c)
	       putStrLn ("Mandel check " ++ show check)

testMandel = do check <- runGraph $ mandelProg 3 3 3
	        putStrLn ("Mandel check " ++ show check)

t = testMandel

main = runMandel

{- 
  NOTES:

[2009.08.12] 
 GHC 6.8.2
 * Params 100 100 2000:
   Mandel check 4810272
    - pure, 1.669 seconds
    - io,   1.466 seconds

 * Params 100 1000 100:
    - pure: stack overflow 
    - io:   stack overflow :(
With 100M stack:  
    - pure: Completes, but mandel check is 0... oh that's the write answer.

 * Params 300 300 100:
    - pure: 9.7 seconds
    - io:   29.29 seconds (with call3/forkIO, uses 67mb) 
             (With call2 it's no better -- 32s !!)

   Running (on a different machine vs 6) on CnC/C++ gives 1.27
   seconds.  Big slow down for haskell on this one, probably
   substantial room for improvement of the inner loop.

By the way, the language shootout threadring compiles with (-O2 -threaded) and runs with:
  +RTS -N5 -qm -qw -RTS 50000000

---------
 Trying GHC 6.10.4:
   * Params 300 300 100:
    - pure: 6.59
    - io:   4.17
  WOW.

    

 -}