-- [2004.08.06]
-- For the moment these are not flattened; they may have complex operands.
-- NesC should be fine with that.
--import Text.PrettyPrint.HughesPJ

module TokenMachine where

type Const = Int
type Id = String
type ConstBind = (Id, Const)
type Token = Id 
type Formal = Id
type TokHandler = (Id, [Formal], Expr)

type Time = Int -- milleseconds

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


