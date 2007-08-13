



/* fifo,string */
write_to_fifo :: (String, String) -> Int =
  foreign("write_string_to_fifo", []);

fun array_to_ptolemy(a) {
  if (a`Array:length == 0) 
  then "{}"
    else  
      "{"++a[0]++
	foldRange(1, a`Array:length-1, "}", fun (x,y) ","++a[y]++x);
}

fun assoc_to_ptolemy((x,y)) {
  x++"="++y
}

fun alist_to_ptolemy(al) {
  if (al`List:length == 0) 
  then "{}"
    else
      "{"++assoc_to_ptolemy(al`head)++
	List:fold1(fun (x,y) x++","++y,
		   List:Map(assoc_to_ptolemy,al`tail));
}
