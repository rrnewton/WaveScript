module TM_simple where

import TM (Const, ConstBind, Time, Id, Token)

type TokHandler = (Token, [Id], Block)

--data Statement = PrimStat
--  deriving (Eq, Show, Read)

data Pgm = Pgm { consts    :: [ConstBind],
		 socconsts :: [ConstBind],
		 socpgm    :: Block,
		 nodetoks  :: [TokHandler],
		 startup   :: [Token]
	       }
  deriving (Eq, Show, Read)

data Prim = Pplus | Pminus | Pmult | Pdiv
	  | Pless | Pgreater | Pleq | Pgeq
	  | Plocdiff | Ploc
--	  | Pflood | Pelectleader
	  | Pdrawmark | Plightup | Prgb
	  --Pamap | Pafold
  deriving (Eq, Show, Read)
	   
data Block = Block { binds :: [Id],
		     stmts :: [Stmt] 
		   }
  deriving (Eq, Show, Read)

data Stmt = Svoid
	  | Sassign Id Basic
          | Sif Basic [Stmt] [Stmt]
	  | Sprimapp Prim [Basic]
          | Ssense
            -- Special forms:
	  | Ssocreturn Basic
	  | Ssocfinished  
	  | Sreturn {val :: Basic,
		     to  :: Token,
		     via :: Token,
		     seed :: Basic,
		     aggr :: Token}
	  | Srelay (Maybe Token)
	  | Semit (Maybe Time) Token [Basic]
	  | Scall (Maybe Time) Token [Basic]
	  | Sactivate Token [Basic]
  deriving (Eq, Show, Read)

data Basic = -- Stndard forms:
            Bconst Const
	  | Bvar Id
  deriving (Eq, Show, Read)
