{- Utility functions -}



module Utils where

import Control.Monad.State 
import Data.Char
import Data.STRef


-----------------------------------------------------------------------------


filter_just []           = []
filter_just (Nothing:t)  = filter_just t 
filter_just (Just x:t)   = x : (filter_just t)

-- This is a utility for picking out the largest contiguous stretch of predicate-satifs
largest_contig pred ls = loop False ls []
    where 
    loop _     []    []                 = Nothing
    loop _     []    acc                = Just $ reverse $ longest acc 
    loop False (h:t) []       | pred h  = loop True  t [[h]]
    loop True  (h:t) (a1:acc) | pred h  = loop True  t ((h : a1) : acc)
    loop False (h:t) (a1:acc) | pred h  = loop True  t ([h] : a1 : acc)
    loop _     (h:t) acc                = loop False t acc


{-
-- A non-tail-recursive version of the same function.
lc2 f ls = longest $ loop ls
    where
    loop []                    = []
    loop [a]                   = if f a then [[a]] else []
    loop (a:b:t) | f a && f b  = (a : head (loop (b:t))) : tail (loop (b:t))
    loop (a:b:t) | f a         = [a] : loop t
    loop (a:t)                 = loop t
-}


longest [] = error "longest: no elements in list"
longest (h:t) = loop h (length h) t
    where loop max len []                      = max
	  loop max len (h:t) | length h > len  = loop h (length h) t
	  loop max len (_:t)                   = loop max len t 

{-loop state acc ls = 
    if null ls
    then (if null acc then Nothing else Just $ reverse acc)
    else if pred $ head ls
    then (if state 
	  then loop True (tail ls) (head ls : acc)
	  else loop True (tail ls) [head ls])Nothing
    else -}

largest_number :: [String] -> Int
largest_number strings =
    let ls = map read $ 
	     filter_just $ 
	     map (largest_contig isDigit)
	     strings
    in if ls == []
       then 0
       else maximum ls

----------------------------------------------------------------------


--init_name_counter :: [String] -> Int
init_name_counter counter many_names = 
    do writeSTRef counter (largest_number many_names)    
--unique_name :: STRef s a -> [Char] -> ST s [Char]
unique_name counter root = 
    do count <- readSTRef counter
       return (root ++ "_" ++ show count)



-- This is an alternative approach to the above monadic one.
-- It returns an infinite list of names.
new_name_stream many_names new_root =
    let loop n = (new_root ++ show (n+1)) : loop (n+1)
    in loop (largest_number many_names)

