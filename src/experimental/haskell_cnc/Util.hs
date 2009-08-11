{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  #-}

module Util where

-- Start = inclusive, End = uninclusive:
foldRange start end acc fn = loop start acc
 where
  loop !i !acc
    | i == end = acc
    | otherwise = loop (i+1) (fn acc i)

-- My own forM:
for_ start end fn = loop start 
 where 
  loop !i | i == end  = return ()
	  | otherwise = do fn i; loop (i+1) 
