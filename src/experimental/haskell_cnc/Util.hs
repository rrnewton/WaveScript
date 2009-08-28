{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  #-}

module Util where

import GHC.Conc
import Control.Concurrent

-- Start = inclusive, End = uninclusive:
foldRange start end acc fn = loop start acc
 where
  loop !i !acc
    | i == end = acc
    | otherwise = loop (i+1) (fn acc i)

-- My own forM:
-- Inclusive start, exclusive end.
for_ start end fn = loop start 
 where 
  loop !i | i == end  = return ()
	  | otherwise = do fn i; loop (i+1) 


-- Split a list into N pieces (not evenly sized if N does not divide
-- the length of the list).
splitN n ls = loop n ls
  where 
    sz = length ls `quot` n
    loop 1 ls = [ls]
    loop n ls = hd : loop (n-1) tl
       where (hd,tl) = splitAt sz ls


-- I'm amazed this is not built-in.
-- We want to run IO threads in parallel and wait till they're done.
forkJoin actions = 
    do joiner <- newChan 
       mapM (\a -> forkIO (do a; writeChan joiner ())) actions
       mapM_ (\_ -> readChan joiner)  actions
       return ()

t = forkJoin [putStrLn "foo", putStrLn "bar", putStrLn "baz"]
