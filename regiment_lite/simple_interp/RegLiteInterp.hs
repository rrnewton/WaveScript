
-- Interpreter

type Space = Int
type Time = Float
type ConstBind = (Id, Expr)

type Const = N Float
	   | B Bool

type Region a = Space -> a
type Signal a = Time -> a

data Id = Id String
  deriving (Eq, Show, Read)


--	  | Plocdiff | Ploc
--	  | Pmyid 
data BinaryOp
  = Eq | Ne | Lt | Gt | Le | Ge | Add | Sub | Mul | Div | Pow 
  | SRfold | SRmap | SRfilter
  | Smap 
  deriving (Eq, Show, Read)

data UnaryOp = Neg | Not
  deriving (Eq, Show, Read)

data Prim a = Unop UnaryOp a
	    | Binop BinaryOp a a
  deriving (Eq, Show, Read)

data Expr = -- Stndard forms:
            Econst Const
	  | Evar Id
	  | Elambda [Id] Expr
	  | Elet [(Id,Expr)] Expr
          | Eif Expr Expr Expr
	  | Eprimapp (Prim Expr)
  deriving (Eq, Show, Read)

data Value a = Cnst Const
	     | Sig a 
	     | Reg a 


temperature_region :: Signal (Region Float)
temperature_region time space = 35.0 


apply_binop Eq v1 v2 = (v1 == v2)

apply_unop (Not (Cnst (B b))) = Cnst (B (not b))


eval_expr :: Env a -> Expr -> Value a
eval_expr env expr = 
    case expr of 
     Econst c -> Cnst c
     Evar id -> case env id of
		 Just x -> x
		 Nothing -> error ("Unbound ID! " ++ show id)
     Eif test conseq altern -> Cnst 3.0
     Eprimapp (Unop op e) -> apply_unop op (eval_expr env e)
     Eprimapp (Binop op e1 e2) ->
	 apply_binop op (eval_expr env e1) (eval_expr env e2)
	 


----------------------------------------
    
type Env a = (Id -> Maybe (Value a))

--empty_env :: Env ()
empty_env n = Nothing

extend_env :: Env a -> Id -> Value a -> Env a
extend_env env n val =
    \ name ->
	if (name == n)
	then Just val
	else env name

apply_env :: Env a -> Id -> Maybe (Value a)
apply_env env name = env name
