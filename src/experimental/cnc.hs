{-
  This first version is serial.
  It uses a single state thread to track all the tag/item collection state.
 -}

import Control.Monad.ST
import Data.STRef
import  Data.Set as Set
import  Data.Map as Map

type Dummy = ()

type TagCol  a   = STRef Dummy (Set a)
type ItemCol a b = STRef Dummy (Map a b)
type Step a = a -> CncCode ()

-- The monad stores all the information in the tag/item collections
--
-- It's a state thread.  There only needs to be ONE state thread.  So
-- I think that means I can use something degenerate for the
-- state-thread-distinguishing type arguument.
type CncCode a = ST Dummy a

--instance Monad CncCode where
--    Hmm [a] >>= f  =  f a
--    return x = Hmm [x]

newTagCol :: CncCode (TagCol a)
newTagCol = newSTRef Set.empty

newItemCol :: CncCode (ItemCol a b)
newItemCol = newSTRef Map.empty

call :: Ord a => TagCol a -> a -> CncCode ()
call col tag = 
    do set <- readSTRef col
       writeSTRef col (Set.insert tag set)


put :: Ord a => ItemCol a b -> a -> b -> CncCode ()
put col tag item = 
    do map <- readSTRef col
       writeSTRef col (Map.insert tag item map)

get :: Ord a => ItemCol a b -> a      -> CncCode b
get col tag = 
    do map <- readSTRef col
       return $ Map.findWithDefault undefined tag map


-- Do we need a different monad for wiring together the graph??
prescribe :: TagCol a -> Step a -> CncCode ()

prescribe = undefined

{-
test = 
    do t1 <- newTagCol
       t2 <- newTagCol
       d1 <- newItemCol
       d2 <- newItemCol
       -- Initialize:
       put d1 'a' 33
       call t1 'a'
       let step1 t = 
	       do val <- get d1 t 
		  put d2 t (val + 1)
		  call t2 t
       return ()

-}

--go = (runST :: (ST Dummy a) -> a) test


--type T = Int
--type V = String
--type Kern a b = (T,[V]) -> ([T],[V])


{- 

ghci -XRankNTypes cnc.hs
ghci -fglasgow-exts cnc.hs

-}