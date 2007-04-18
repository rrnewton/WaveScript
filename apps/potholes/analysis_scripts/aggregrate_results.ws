

base = "/home/newton/data/"
files = [ base ++ "slave10.txt.dstlog",
	  base ++ "slave14.txt.dstlog" ]

streams = map(fun (f) (readFile), files);

derivs = iterate (ind, (t,timpsteps,unique)) in unionList(streams) {
  
}

iterate ls in unionList(streams) {
  state { curtime  }
  List:insert_sorted();
   
}
