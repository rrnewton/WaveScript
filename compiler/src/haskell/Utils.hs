{- Utility functions -}



module Utils where

import Data.Char

-----------------------------------------------------------------------------

longest [] = error "longest: no elements in list"
longest (h:t) = loop h (length h) t
    where loop max len []                      = max
	  loop max len (h:t) | length h > len  = loop h (length h) t
	  loop max len (_:t)                   = loop max len t 

filter_just []           = []
filter_just (Nothing:t)  = filter_just t 
filter_just (Just x:t)   = x : (filter_just t)

{-loop state acc ls = 
    if null ls
    then (if null acc then Nothing else Just $ reverse acc)
    else if pred $ head ls
    then (if state 
	  then loop True (tail ls) (head ls : acc)
	  else loop True (tail ls) [head ls])Nothing
    else -}


largest_contig pred ls = loop False ls []
    where 
    loop _     []    []                 = Nothing
    loop _     []    acc                = Just $ reverse $ longest acc 
    loop False (h:t) []       | pred h  = loop True  t [[h]]
    loop True  (h:t) (a1:acc) | pred h  = loop True  t ((h : a1) : acc)
    loop False (h:t) (a1:acc) | pred h  = loop True  t ([h] : a1 : acc)
    loop _     (h:t) acc                = loop False t acc



-- A non-tail-recursive version of the same function.
lc2 f ls = longest $ loop ls
    where
    loop []                    = []
    loop [a]                   = if f a then [[a]] else []
    loop (a:b:t) | f a && f b  = (a : head (loop (b:t))) : tail (loop (b:t))
    loop (a:b:t) | f a         = [a] : loop t
    loop (a:t)                 = loop t


----------------------------------------------------------------------

-- This is an alternative approach to the above monadic one:

new_name_stream many_names new_root =
    let loop n = (new_root ++ show (n+1)) : loop (n+1)
    in loop (maximum $ 
	      map read $ 
	       filter_just $ 
	        map (largest_contig isDigit) 
                    many_names)

