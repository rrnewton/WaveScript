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

num_nodes = 4

static_domain :: [Space] = [1..num_nodes]

random_world :: IO World
random_world = 
    do xs <- sequence $ replicate num_nodes $ randomRIO (0,100::Int)
       ys <- sequence $ replicate num_nodes $ randomRIO (0,100::Int)   
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

data UnaryOp = Neg | Not | SenseLight | SenseTemp
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

-- I needn't represent closures as functions here, but I figure we've
-- already got non-printable types in the Signals and Regions.
data Value = VUnit
	   | VNum Const
	   | VNode SimNode
	   | VSig (Signal Value)
	   | VSReg (SRegion Value)
	   | VFun (Value -> Value)
--  deriving (Eq, Show, Read)
	       
instance Show Value where
    show (VNum c) = show c
    show (VSig s) = show $ map s [0..10]
    show (VSReg sr) = concat
		     $ map (\ r -> show (map r static_domain) ++ "\n") 
		     $ map sr [0..10]
    show (VFun f) = "<funV>"

apply_unop Not (VNum (B b)) = VNum (B (not b))
apply_unop SenseLight (VNode nd) = VNum (N (light nd))
apply_unop SenseTemp  (VNode nd) = VNum (N (temp nd))

apply_binop :: BinaryOp -> Value -> Value -> Value
apply_binop Eq (VNum v1) (VNum v2) = VNum (B (v1 == v2))
apply_binop Smap (VFun f) (VSig s) = VSig (\t -> f (s t))
apply_binop SRmap (VFun f) (VSReg r) = VSReg (\t id -> f (r t id))
apply_binop Add (VNum (N x)) (VNum (N y)) = VNum (N (x + y))



apply_trinop :: TrinaryOp -> Value -> Value -> Value -> Value
apply_trinop SRfold (VFun f) v (VSReg r) = 
    VSig (\t ->
	  let g (a::Value) (b::Value) = 
	        case f a of 
	          VFun f2 -> f2 b
	          v -> error ("Two argument function only took one value, leaving " ++ show v)
	  in 
	    foldl g v
	          ((snapshot_region $ r t) :: [Value]))

--(foldl g v (snapshot_region (r t)))

eval_expr :: World -> Env -> Expr -> Value
eval_expr world env expr = 
    let loop = eval_expr world env in
    case expr of 
     Econst c -> VNum c
     Evar id -> case env id of
		 Just x -> x
		 Nothing -> error ("Unbound ID! " ++ show id)
     Eif test conseq altern -> VNum (N 3.0)
     Eprimapp (Unop op e) -> apply_unop op (loop e)
     Eprimapp (Binop op e1 e2) ->
	 apply_binop op (loop e1) (loop e2)
     Eprimapp (Trinop op e1 e2 e3) ->
	 apply_trinop op (loop e1) (loop e2) (loop e3)
     Eprimapp (Zerop World) -> VSReg (\ t s -> VNode (world t s))
     Elet binds body -> 
	 let newenv = foldl (\ accenv (lhs,rhs) ->
			     (extend_env accenv lhs (loop rhs)))
		      env binds
	 in eval_expr world newenv body
     Elambda vars body ->
         -- Form a closure, do currying:
	 case vars of
	   [] -> VFun (\ _ -> loop body)   -- Zero args passes unit instead.
	   [v] -> VFun (\ val -> eval_expr world (extend_env env v val) body)
	   v1:tl -> VFun (\ val -> 
			  eval_expr world (extend_env env v1 val)
			  (Elambda tl body))
		 

-- Eval expression in empty environment and print result.
eval :: Expr -> IO Value
eval e = do w <- random_world
	    let v = eval_expr w empty_env e
	    putStr (show v ++ "\n")
	    return v

-- srmap senseLight world
e0 = Eprimapp 
     (Binop SRmap 
      (Elambda [Id "n"]
       (Eprimapp (Unop SenseLight (Evar (Id "n")))))
      (Eprimapp (Zerop World)))

-- srfold (+) 0 (srmap senseLight world)
e1 = Eprimapp (Trinop SRfold 
	       (Elambda [Id "acc", Id "n"] 
		(Eprimapp (Binop Add (Evar (Id "acc")) (Evar (Id "n")))))
               (Econst (N 0.0))
	       e0)




----------------------------------------
    
type Env = (Id -> Maybe (Value))

--empty_env :: Env ()
empty_env n = Nothing

extend_env :: Env -> Id -> Value -> Env
extend_env env n val =
    \ name ->
	if (name == n)
	then Just val
	else env name

apply_env :: Env -> Id -> Maybe (Value)
apply_env env name = env name
