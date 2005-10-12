
test = 3

-- <Pgm>  ::= <Exp>
-- <Decl> ::= (<var> <Exp>)
-- <Exp>  ::= 
--            (quote <datum>)
--          | <constant>
--          | <var>
--          | (if <Exp> <Exp> <Exp>)
--          | (lambda <Formalexp> <Exp>)
--          | (letrec (<Decl>*) <Exp>)
--          | (<primitive> <Exp>*)
-- <Formalexp> ::= (<var>*)

--type Expr = Int


data Prim a = Pcluster a 
	  | Pamap 
	  | Pafold 
	  | Psmap 
	  | Punion 
	  | Pintersect 
  deriving (Eq, Show, Read)







type Var = String


--type Prim = Rmap | Smap 
--	  | Circle | CircleAt
--	  | Cons | Plus | Minus | Times | Divide
	    
--type Expr = imm Int
--	  | var var
--	  | Lambda of ((var list) * expr)
--	  | Let of ((var * expr) * expr)
--	  | Primapp of (prim * (expr list))
--	  | If of (expr * expr * expr)

-- type Core = Pgm of expr


--let x = Pgm (Let (("r", Primapp 
--		     (CircleAt,
--		      [Imm 50; 
--		       Primapp (Cons, [Imm 30; Imm 40])])),
--		  Var "r"));;       	

