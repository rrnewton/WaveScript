import HUnit 


data DensityT = Bottom 
	      | LocalVal
	      | SparseArea DensityT
	      | ContigArea DensityT
	      | Top
  deriving (Eq, Show, Read)

data Prim a = Pcluster a 
	  | Pamap 
	  | Pafold 
	  | Psmap 
	  | Punion 
	  | Pintersect 

lattice_compare a b | a == b = Just EQ

lattice_compare Bottom   _    = Just LT
lattice_compare  _      Top   = Just LT
lattice_compare (SparseArea a) (SparseArea b) = lattice_compare a b 
lattice_compare (ContigArea a) (ContigArea b) = lattice_compare a b 
lattice_compare  _  _         = Nothing


transfer (Pcluster dens) = ContigArea dens


data Expr = Elambda 
	  | Eprimapp 

