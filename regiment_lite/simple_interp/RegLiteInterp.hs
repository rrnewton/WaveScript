-- RRN: Started [2005.03.12]

module Main where

import Data.List
import System.Random
import Data.Graph.Inductive
import System.IO.Unsafe

main :: IO ()
main = 
    do w <- random_world
       let g = comm_graph w
       putStr $ show (g 0)
       putStr "\n"
       return ()

type Region a = Space -> a
type Signal a = Time -> a

type SRegion a = Signal (Region a)

type Space = Int
type Time = Float

----------------------------------------------------------------------

data SimNode = Nd { nid :: Int,
		    nx, ny :: Int,
		    temp,light :: Float}
  deriving (Eq, Show, Read)


type World = SRegion SimNode --[SimNode]

static_domain :: [Space] = [1..30]

random_world :: IO World
random_world = 
    do xs <- sequence $ replicate 30 $ randomRIO (0,100::Int)
       ys <- sequence $ replicate 30 $ randomRIO (0,100::Int)   
       gen <- getStdGen 
       let temps  = randomRs (10,90::Float) gen
           lights = randomRs (0,1::Float) gen
       return (\ t id ->
	       Nd id (xs!!(id-1)) (ys!!(id-1))
	          (temps!!(floor t)) (lights!!(floor t)))

--       let nds = map ( \ (id,x,y,t,l) -> Nd id x y t l)
--	         $ zip5 [1..] xs ys temps lights
--       return nds


-- We don't actually use any topology related stuff yet.
-- In the near future might define aggregation more realistically to have time-skew,
-- which will require generating network topologies.  
-- (This below just makes a graph from a super-simple disc radio model.)

dist :: SimNode -> SimNode -> Float
dist n1 n2 = sqrt (fromIntegral $ (nx n1 - nx n2)^2 + (ny n1 - ny n2)^2)

can_communicate :: SimNode -> SimNode -> Bool
can_communicate n1 n2 = dist n1 n2 < 15.0

snapshot_region :: Region a -> [a]
snapshot_region r = [ r x | x <- static_domain ]

comm_graph :: World -> Signal (Gr SimNode ())
comm_graph w t = 
    let snapshot = snapshot_region (w t)
        f n1 (_,n2) = can_communicate n1 n2 && n1 /= n2 
    in
	foldl (\ g nd -> 
	       let sofar = labNodes g 
	           newlab::Node = if sofar==[]
	                          then 1 
	                          else 1 + maximum (map fst sofar)
	           neighbors = map (\ (i,nd) -> ((),i))
	                       $ filter (f nd) sofar
		in 
	       ((neighbors, newlab,nd, neighbors) -- bidirectional links
	        & g))
	empty snapshot


----------------------------------------------------------------------
-- Interpreter

type ConstBind = (Id, Expr)

data Const = N Float
	   | B Bool
  deriving (Eq, Show, Read)


data Id = Id String
  deriving (Eq, Show, Read)


--	  | Plocdiff | Ploc
--	  | Pmyid 


data ZeroOp = World 
  deriving (Eq, Show, Read)

data UnaryOp = Neg | Not | SenseLight
  deriving (Eq, Show, Read)

data BinaryOp
  = Eq | Ne | Lt | Gt | Le | Ge | Add | Sub | Mul | Div | Pow 
  | SRmap | SRfilter | Smap 
  deriving (Eq, Show, Read)

data TrinaryOp = SRfold
  deriving (Eq, Show, Read)


data Prim a = Unop UnaryOp a
	    | Binop BinaryOp a a
	    | Trinop TrinaryOp a a a
	    | Zerop ZeroOp
  deriving (Eq, Show, Read)

data Expr = -- Stndard forms:
            Econst Const
	  | Evar Id
	  | Elambda [Id] Expr
	  | Elet [(Id,Expr)] Expr
          | Eif Expr Expr Expr
	  | Eprimapp (Prim Expr)
  deriving (Eq, Show, Read)

data Value = Cnst Const
	     | Sig (Signal Value)
	     | SReg (SRegion Value)
	     | Fun (Value -> Value)
--  deriving (Eq, Show, Read)


apply_unop Not (Cnst (B b)) = Cnst (B (not b))


apply_binop :: BinaryOp -> Value -> Value -> Value
apply_binop Eq (Cnst v1) (Cnst v2) = Cnst (B (v1 == v2))
apply_binop Smap (Fun f) (Sig s) = Sig (\t -> f (s t))
apply_binop SRmap (Fun f) (SReg r) = SReg (\t id -> f (r t id))


apply_trinop :: TrinaryOp -> Value -> Value -> Value -> Value
apply_trinop SRfold (Fun f) v (SReg r) = 
    Sig (\t ->
	 let g (a::Value) (b::Value) = 
	        case f a of 
	          Fun f2 -> f2 b
	          _ -> error "Two argument function only took one value."
	 in 
	    foldl g v
	          ((snapshot_region $ r t) :: [Value]))

--(foldl g v (snapshot_region (r t)))

eval_expr :: Env a -> Expr -> Value
eval_expr env expr = 
    case expr of 
     Econst c -> Cnst c
     Evar id -> case env id of
		 Just x -> x
		 Nothing -> error ("Unbound ID! " ++ show id)
     Eif test conseq altern -> Cnst (N 3.0)
     Eprimapp (Unop op e) -> apply_unop op (eval_expr env e)
     Eprimapp (Binop op e1 e2) ->
	 apply_binop op (eval_expr env e1) (eval_expr env e2)
     Eprimapp (Trinop op e1 e2 e3) ->
	 apply_trinop op (eval_expr env e1) (eval_expr env e2) (eval_expr env e3)



e1 = Eprimapp (Trinop SRfold 
	              (Elambda [] (Cnst 3.0)) 
                      0.0 
	              (Binop SRmap 
		             (Zerop SenseLight) 
		             (Zerop World)))



----------------------------------------
    
type Env a = (Id -> Maybe (Value))

--empty_env :: Env ()
empty_env n = Nothing

extend_env :: Env a -> Id -> Value -> Env a
extend_env env n val =
    \ name ->
	if (name == n)
	then Just val
	else env name

apply_env :: Env a -> Id -> Maybe (Value)
apply_env env name = env name
