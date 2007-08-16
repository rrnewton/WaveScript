



/* fifo,string */
c_write_to_file :: (Int, String, String) -> Int =
  foreign("write_string_to_file", []);


fun write_to_fifo(fifo, str) {
  c_write_to_file(1, fifo++nullterm, str++nullterm)
}

fun write_to_file(fifo, str) {
  c_write_to_file(0, fifo++nullterm, str++nullterm)
}



fun array_to_ptolemy(a) {
  if (a`Array:length == 0) 
    then "{}"
      else  
	foldRange(0, a`Array:length-2, 
          "{", fun (x,y) x++a[y]++",")++
	    a[a`Array:length-1]++"}";
}

fun assoc_to_ptolemy((x,y)) {
  x++"="++y
}

fun alist_to_ptolemy(al) {
  using List;
  assocs = reverse$ map(assoc_to_ptolemy, al);
  if (al`List:length == 0) 
  then "{}"
  else {
    "{"++
      fold(fun (acc,y) y ++", "++ acc,
           assocs`head ++ "}",
           assocs`tail)
  }
}
