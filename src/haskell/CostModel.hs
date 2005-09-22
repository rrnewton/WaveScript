-- ONLY A SKETCH:
-- Prototyping...

--import HUnit 
import TokenMachine

{- 
  Wondering about how they do Id's in ghc... seems pretty complicated.

-}

data DensityT = Bottom 
	      | LocalVal
	      | SparseArea DensityT
	      | ContigArea DensityT
	      | Top
  deriving (Eq, Show, Read)

--type Id = String
--data Token = Token Id 
--type Tokbind = (Token, [Id], Expr)
--data Pgm = Pgm Tokbind
--data Tokbind = Tokbind Token [Id]
--type Formals = [String]


lattice_compare a b | a == b = Just EQ

lattice_compare Bottom   _    = Just LT
lattice_compare  _      Top   = Just LT
lattice_compare (SparseArea a) (SparseArea b) = lattice_compare a b 
lattice_compare (ContigArea a) (ContigArea b) = lattice_compare a b 
lattice_compare  _  _         = Nothing


transfer (Pcluster dens) = ContigArea dens




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

main = do return ();



data Point = Pt {pointx, pointy :: Float}
  deriving (Eq, Show, Read)

