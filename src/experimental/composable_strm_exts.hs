

-- I'm experimenting with various ways to encode extensions to
-- streaming operators such as the following;
--  * Teleporting message support
--  * Pull-based streams
--  * Migration support
--  * Self-profiling
--  * (maybe) explicit rates

-- Could use arrows, but they don't quite capture asynchronous,
-- discrete-event dataflow properly.  (How do you do nondeterministic
-- merge?  How do you represent variable numbers of output elements
-- per input element?)

-- [2009.02.22] The best we could possibly hope for in terms of
-- composability is something like monad transformers.  We can't
-- expect commutativity.  Adding pull support adds back channels,
-- adding teleporting second would add extr messages on those
-- backchannels as well.  In the other order the back-channels would
-- be untouched.

data StateOp a b = 
--  StateOp (forall sig. (sig, (a,sig) -> ([b],sig)))
  StateOp (a -> ([b], StateOp a b))
--  StateOp (forall sig. (sig, (a,sig) -> ([b], StateOp a b)))

class StreamT t where 
--  bind :: t a -> (a -> t b) -> t b -- monads
--  bind :: t a -> (a -> [b]) -> t b
  bind :: t a -> StateOp a b  -> t b

newtype Stream a = Stream [a]
  deriving Show

instance StreamT Stream where 
--  bind x = bind x
--  bind (Stream x) f = Stream( concat $ map (\o -> let Stream b = f o in b) x)
--  bind (Stream x) f = Stream$ concat $ map f x
  bind (Stream ls) op = Stream $ loop ls op
    where 
      loop [] _ = []
      loop (h:t) (StateOp f) =
	  let (bs,op') = f h
	  in bs ++ loop t op'
	       
--     scanl (\ (StateOp f) x -> 
-- 	   let (bs,f') = f x 
-- 	   in 
-- 	   ) op ls

s = Stream [1,2,3]
-- t2 = bind test (\x -> [x,x])


--a :: StateOp Int Int
--a = StateOp (0, \ (n,count) -> ([n+count], count+1))
a count = StateOp (\n -> ([n+count], a$count+1))
-- Here's one that's really stateless:
b = StateOp (\n -> ([n,n], b))

pipe = s `bind` a 100 `bind` b

--------------------------------------------------------------------------------

-- Teleporting messages:
-- The real implementation of teleporting messages should use records
-- to pack the message datatype.  For now, there's either a message or
-- no message.
type TeleMsg a = Maybe a 

-- The transformer, lifts a StateOp that doesn't consume or produce
-- teleporting messages.  This is like Arrow "first", but a little
-- trickier.  Messages must be preserved, so if no output is produced
-- on an execution, then we must store the message.  However, messages
-- can be subsumed by newer messages.
teleliftOp (StateOp f) = loop Nothing f
  where 
  or Nothing (Just x) = Just x
  or (Just x) Nothing = Just x
  or x _ = x
  loop stored f = StateOp $ 	       
    \(a,m) -> 
       let (bs, StateOp f') = f a
	   new = or m stored
       in case bs of 
	   [] -> ([], loop new f')
	   -- The first message out gets the tag.
           (h:t) -> ((h,new) : zip t (repeat Nothing), loop Nothing f')

teleliftS (Stream ls) = Stream $ zip ls (repeat Nothing)

-- This is the start of a chain of ops that pass teleporting messages.
--telesender

c = StateOp (\(n,msg) -> ([(n, Just "hello")], c))

-- And this is the end.
--telereceiver

-- On the way out, we use this to send:
telesend msg val = (val,msg)

--s2 = Stream [(1,Nothing), (2,Nothing), (3,Nothing)]
s2 = teleliftS s

p2 = s2 `bind` teleliftOp (a 100) `bind` c `bind` teleliftOp b

newtype TeleStrm a = TeleStrm (Stream (a,Maybe [Char]))

--instance StreamT TeleStrm where 
--  bind (TeleStrm s) op = TeleStrm $ bind (teleliftS s) op
    
--------------------------------------------------------------------------------

-- Self profiling is similar to teleporting messages in that it just
-- adds another field to the output where profile data is emitted.

--------------------------------------------------------------------------------

-- Pull streams.

newtype PullStream a = PullStream (Stream () -> Stream a)

-- This one really changes the structure of the graph, adding edges
-- and thereby cycles.  That's tricky because it's not just a
-- one-to-one transform on StateOps.  The sources have to be special
-- operators that already support the protocol.

-- The way I implemented pull streams, they defer wiring their
-- "pullstring" (i.e. they are functions that take a pullstring).  How
-- should that late wiring work here?  Transforming a stateOp means
-- producing something that has two inputs (and is a different kind):
-- the pull requests and the normal elements.

-- This means `bind` will have to be different:

instance StreamT PullStream where 
  bind (PullStream f) op = PullStream $
       \ pulls -> Stream $ loop (deStrm (f pulls)) op
         where 
	 deStrm (Stream ls) = ls
 	 loop [] _ = []
         loop (h:t) (StateOp opf) = 
 	   let (bs,op') = opf h  
   	   in bs ++ loop t op'
 
s3 = PullStream $ (\_ -> s)

p3 = pop (s3 `bind` b `bind` b)

pop (PullStream f) = f (Stream (repeat ()))

--------------------------------------------------------------------------------
