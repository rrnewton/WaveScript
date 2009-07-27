{-
  This first version is serial.
  It uses a single state thread to track all the tag/item collection state.
 -}

import Control.Monad.ST
import Data.STRef
import  Data.Set as Set
import  Data.Map as Map

import System.IO.Unsafe

type Dummy = ()

-- A tag collection has a buffer of newly written tags.
-- It also has a unique identifier.
type TagColID = Int
type TagCol  s a   = STRef s (TagColID, Set a, Set a)

type ItemCol s a b = STRef s (Map a b)
type Step s a = a -> CncCode s ()

-- The monad stores all the information in the tag/item collections
--
-- It's a state thread.  There only needs to be ONE state thread.  So
-- I think that means I can use something degenerate for the
-- state-thread-distinguishing type arguument.
type CncCode s a = ST s a

--instance Monad CncCode where
--    Hmm [a] >>= f  =  f a
--    return x = Hmm [x]

counter = newSTRef 0

newTagCol :: TagColID -> CncCode s (TagCol s a)
newTagCol cnt = 
            do --tmp <- counter 
	       --cnt <- readSTRef tmp
	       --writeSTRef tmp (cnt+1)
	       newSTRef (cnt+1, Set.empty,Set.empty)

newItemCol :: CncCode s (ItemCol s a b)
newItemCol = newSTRef Map.empty

call :: Ord a => TagCol s a -> a -> CncCode s ()
call col tag = 
    do (id, frontset,backset) <- readSTRef col
       writeSTRef col (id, Set.insert tag frontset, backset)


put :: Ord a => ItemCol s a b -> a -> b -> CncCode s ()
put col tag item = 
    do map <- readSTRef col
       writeSTRef col (Map.insert tag item map)

get :: Ord a => ItemCol s a b -> a      -> CncCode s b
get col tag = 
    do map <- readSTRef col
       return $ Map.findWithDefault undefined tag map


type MatchedPair s = (forall t. Ord t => (TagCol s t, Step s t))
--type Graph s = Ord TagColID => STRef s (forall t. Ord s => Map TagColID [Step s t])
--type Graph s = STRef s (forall t. Ord t => Map TagColID [Step s t])
--type Graph s = STRef s (Map TagColID (forall t. Ord t => [Step s t]))
data Graph s = Graph (STRef s (Map TagColID [MatchedPair s]))
--type Graph s = forall t. Ord t => STRef s (Map TagColID ([Step s t]))

-- Do we need a different monad for wiring together the graph??
-- Not in this version...

-- A graph data structure keeps track of the steps that are prescribed
-- everything that is needed by the scheduler.
newGraph :: CncCode s (Graph s)
newGraph = do ref <- newSTRef (Map.empty :: Map TagColID [MatchedPair s])
	      return (Graph ref)

--STRef s a -> t -> t1 -> ST s ()
-- HUH -- why Ord s and not ord t???
--prescribe :: Ord s => Graph s -> TagCol s t -> Step s t -> CncCode s ()
--prescribe :: Ord tg => Graph s -> TagCol s tg -> Step s tg -> CncCode s ()

--prescribe :: Graph s -> TagCol s tg -> Step s tg -> CncCode s ()

--forall s t1 a t2 t3 t.
--             (Ord t1) =>
--             STRef s (Map t1 [a]) -> STRef s (t1, t2, t3) -> t -> ST s ()

--prescribe :: forall s t a t1 t2.
--             (Ord t) =>
--             STRef s (Map t [a]) -> STRef s (t, t1, t2) -> a -> ST s ()

--             STRef s (Map t [a]) -> STRef s (t, t1, t2) -> a -> ST s ()

--prescribe :: 
--	     STRef s (Map t [a]) -> STRef s (t, t1, t2) -> a -> CncCode s ()
---	     Graph s             -> STRef s (t, t1, t2) -> a -> CncCode s ()
--	     STRef s (Map t [a]) -> TagCol s t1         -> a -> ST s ()
--	     STRef s (Map TagColID [a]) -> TagCol s t1  -> a -> ST s ()
--	     STRef s (Map TagColID [(forall t. Ord t => Step s t)]) -> TagCol s t1  -> a -> ST s ()

--prescribe :: Graph s -> TagCol s tg -> Step s tg -> CncCode s ()
prescribe :: Graph s -> MatchedPair s -> CncCode s ()
--prescribe (Graph graph) tagc (step) = 
prescribe (Graph graph) (tagc,step) = 
    do stepmap <- readSTRef graph
       (id,frnt,bck) <- readSTRef tagc
       let ls = Map.findWithDefault ([]::[MatchedPair s]) id stepmap 
--       writeSTRef graph stepmap
--       writeSTRef graph (Map.insert id ls stepmap)
--       writeSTRef graph (Map.insert id [step] stepmap)
--       let pr :: forall s. MatchedPair s = (tagc,step) 
       let pr = (tagc,step) 
--       writeSTRef graph (Map.insert id (pr:ls) stepmap)
--       return stepmap
--       return pr
       return ()


-- System.IO.Unsafe.unsafePerformIO


test = 
    do t1 <- newTagCol 0
       t2 <- newTagCol 1
       d1 <- newItemCol
       d2 <- newItemCol
       -- Initialize:
       put d1 'a' 33
       call t1 'a'
       let step1 t = 
	       do val <- get d1 t 
		  put d2 t (val + 1)
		  call t2 t
       sched <- newGraph
--       prescribe sched t1 step1
       return ()


--go = runST test


--type T = Int
--type V = String
--type Kern a b = (T,[V]) -> ([T],[V])


{- 

ghci -XRankNTypes cnc.hs
ghci -fglasgow-exts cnc.hs

-}