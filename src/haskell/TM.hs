-- Token Machines

-- [2004.08.06]
-- For the moment these are not flattened; they may have complex operands.
-- NesC should be fine with that.
--import Text.PrettyPrint.HughesPJ

module TM where

type Const = Int
type Id = String
type ConstBind = (Id, Const)
--type Formal = Id
type TokHandler = (Token, [Id], Expr)

data Id = Id String
  deriving (Eq, Show, Read)

data Token = Token String
  deriving (Eq, Show, Read)

--data Statement = PrimStat
--  deriving (Eq, Show, Read)

data Expr = Ereturn Id 
	  | Etoken Id
	  | Eprimapp
	  | Eassign
	  | Ecall Id [Expr]
	  | Etimedcall Id Time [Expr]
	  | Eemit
  deriving (Eq, Show, Read)

--  deriving (Eq, Show, Read, Doc)

data TMPgm = Pgm { consts    :: [ConstBind],
	      socconsts :: [ConstBind],
	      socpgm    :: [Expr],
	      nodetoks  :: [TokHandler],
	      startup   :: [Token]
	    }
  deriving (Eq, Show, Read)

data Expr = -- Stndard forms:
            Econst
	  | Evar Id
	  | Elambda [Id] Expr
	  | Eprimapp (Prim Expr)
            -- Special forms:
	  | Esocreturn Expr
	  | Esocfinished	  
	  | Ereturn Expr
	  | Erelay (Maybe Token)
	  | Eemit
	  | Ecall
  deriving (Eq, Show, Read)


data Prim a = Pcluster a 
	  | Pamap 
	  | Pafold 
	  | Psmap 
	  | Punion 
	  | Pintersect 
  deriving (Eq, Show, Read)



x = Pgm { consts    = [],
	  socconsts = [],
	  socpgm    = [],
	  nodetoks  = [],
	  startup   = []
	}

y = Pgm [] [] [] [] []


z = Pgm { consts    = [("woot",3)],
	  socconsts = [("foot",4)],
	  socpgm    = [],
	  nodetoks  = [("tok1", ["x"], Ereturn "x")],
	  startup   = []
	}


a = (Pgm {
  consts = [(result_2, (Econst 3))],
  socconsts=[],
  socpgm=[(Esocreturn (Evar (Id "result_2"))), Esocfinished],
  nodetoks=[((Token "spread-global"), [], (Ebegin [(Eemit Nothing (Token "global-tree") []), (Ecall (Just 1000) (Token "spread-global") [])])), ((Token "global-tree"), [], (Erelay Nothing))],  startup=[]
})

s = "(Pgm {"


--  consts = [(result_2, (Econst 3))],
--  socconsts=[],
--  socpgm=[(Esocreturn (Evar (Id \"result_2\"))), Esocfinished],
--  nodetoks=[((Token \"spread-global\"), [], (Ebegin [(Eemit Nothing (Token \"global-tree\") []), (Ecall (Just 1000) (Token \"spread-global\") [])])), ((Token \"global-tree\"), [], (Erelay Nothing))],  startup=[]
--})"
