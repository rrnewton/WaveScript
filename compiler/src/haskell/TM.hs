-- Token Machines

-- [2004.08.06]
-- For the moment these are not flattened; they may have complex operands.
-- NesC should be fine with that.
--import Text.PrettyPrint.HughesPJ

module TM where

type Const = Int
type ConstBind = (Id, Expr)
--type Formal = Id
type TokHandler = (Token, [Id], Expr)
type Time = Int


data Id = Id String
  deriving (Eq, Show, Read)

data Token = Token String
  deriving (Eq, Show, Read)

--data Statement = PrimStat
--  deriving (Eq, Show, Read)

data TMPgm = Pgm { consts    :: [ConstBind],
	      socconsts :: [ConstBind],
	      socpgm    :: [Expr],
	      nodetoks  :: [TokHandler],
	      startup   :: [Token]
	    }
  deriving (Eq, Show, Read)

data Expr = -- Stndard forms:
            Econst Const
	  | Evar Id
	  | Elambda [Id] Expr
	  | Eseq Expr Expr
--	  | Eprimapp (Prim Expr)
            -- Special forms:
	  | Esocreturn Expr
	  | Esocfinished	  
	  | Ereturn Expr
	  | Erelay (Maybe Token)
	  | Eemit (Maybe Time) Token [Expr]
	  | Ecall (Maybe Time) Token [Expr]
  deriving (Eq, Show, Read)


--data UNOP = 
data BINOP = TMPplus | TMPmult
--data TRIOP = 




x = Pgm { consts    = [],
	  socconsts = [],
	  socpgm    = [],
	  nodetoks  = [],
	  startup   = []
	}

y = Pgm [] [] [] [] []


z = Pgm { consts    = [(Id "woot",Econst 3)],
	  socconsts = [(Id "foot",Econst 4)],
	  socpgm    = [],
	  nodetoks  = [(Token "tok1", [Id "x"], Ereturn (Evar (Id "x")))],
	  startup   = []
	}

s = "(Pgm {} " ++ 
    ")"

r = Econst
    3

doit = do s <- readFile "test.tm";
	  putStr "Read file:\n";
	  putStr s;
	  let stuff = ((read s) :: TMPgm)
--	  let stuff = 3593
	  putStr (show stuff);
	  putStr "\n That's that...\n"
	  return stuff


--  consts = [(result_2, (Econst 3))],
--  socconsts=[],
--  socpgm=[(Esocreturn (Evar (Id \"result_2\"))), Esocfinished],
--  nodetoks=[((Token \"spread-global\"), [], (Ebegin [(Eemit Nothing (Token \"global-tree\") []), (Ecall (Just 1000) (Token \"spread-global\") [])])), ((Token \"global-tree\"), [], (Erelay Nothing))],  startup=[]
--})"


a = (Pgm {
  consts = [((Id "result_1"), (Econst 3))],
  socconsts=[],
  socpgm=[(Esocreturn (Evar (Id "result_1"))), Esocfinished],
  nodetoks=
    [((Token "spread-global"), 
      [], 
      (Eseq (Eemit Nothing (Token "global-tree") []) 
       (Ecall (Just 1000) (Token "spread-global") []))), 
     ((Token "global-tree"), [], 
      (Erelay Nothing))],  
  startup=[]
    })
