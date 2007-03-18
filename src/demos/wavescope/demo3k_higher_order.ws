
s0 = audioFile("./countup.raw", 4096, 0, 44000);

fun println(s) {
  print("  ");
  print(s);
  print("\n");
};

BASE <- iterate(w in s0) {  
  arr = toArray(subseg(w, w.start, 20));
  ls = Array:toList(arr);

  println("OrigWindow[5]: " ++ w[[5]]);

  println("\nList: " ++ ls);
  println("List[5]: " ++ ls.List:ref(5) );
  println("Mapped: " ++ List:map(fun(x) x /_ 10, ls));
  println("Map null: " ++
	  ((List:map(fun(x) x /_ 10, ([]::List Int))) :: List Int));
  println("Folded: " ++ List:fold((+), 1, ls));

  println("\nArr: " ++ arr);
  println("Arr[5]: " ++ arr[5]);

  // Don't have array equality in WSC:
  //  println("Mapped: " ++ Array:map(fun(x) x /_ 10, arr));
  //  println("Map null: " ++
  //	  ((Array:map(fun(x) x /_ 10, (Array:null :: Array Int))) :: Array Int));
  println("Folded: " ++ Array:fold((+), 1, arr));
  println("AndMapped: " ++ Array:andmap(fun(x) x > 400, arr));

  println("Build StaticElab: " ++ Array:build(10, fun (x) x*10));
  println("Build Dynamic: " ++ Array:build(w.width /_ 400, fun (x) x*10));

  emit ();
}
