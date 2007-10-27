
//s0 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int));
s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));

//	       "mode: binary  rate: 44000  repeats: 0 "++
//	       "skipbytes: 2  window: 50 offset: 2")


//s1 = deep_smap(int16ToInt, s0);

fun assert(str,b) if not(b) then wserror("Assert failed: "++ str ++"\n");
fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);

fun assert_prnt(str,a,b) {
  assert_eq(str,a,b);
  print("Assert passed: "++ str ++ "\n");
}


fun println(str) {
  print("  ");
  print(str);
  print("\n");
};

// TODO: Test meta-time higher order also!!!
metals  =  List:build(10, fun(x)x);
metaarr = Array:build(10, fun(x)x);

BASE <- iterate(w in s1) {  
  arr = toArray(subseg(w, w.start, 20));
  ls = Array:toList(arr);

  assert_prnt("meta built eq len", metals`List:length, metaarr`Array:length);
  assert_prnt("meta built eq", metals, metaarr`Array:toList);

  println("OrigWindow[5]: " ++ w[[5]]);

  {
    using List;
    println("\nList: " ++ ls);

    assert_prnt("ls[5] eq w[5]", ls`List:ref(5), w[[5]]);

    mapped = map(fun(x) x / gint(10), ls);
    assert_prnt("mapped len", mapped`length, ls`length);

    built = build(mapped`length, fun(i) List:ref(mapped,i));

    assert_prnt("built eq", built, mapped);

    println("Mapped: " ++ mapped);
    println("Convert: " ++ map(fun(x) int16ToFloat (x / gint(10)), ls));

    println("Map null: " ++
	    ((map(fun(x) x /_ 10, ([]::List Int))) :: List Int));
    println("Folded: " ++ fold((+), gint(1), List:map(int16ToInt, ls)));
  };

  {
    println("\nArr: " ++ arr);
    println("Arr[5]: " ++ arr[5]);

    // Don't have array equality in WSC:
    //  println("Map null: " ++
    //	  ((Array:map(fun(x) x /_ 10, (Array:null :: Array Int))) :: Array Int));
    println("Mapped: " ++ Array:map( (/ gint(10)), arr));
    println("Convert: " ++ Array:map( fun(x) int16ToFloat (x / gint(10)), arr));
    println("Folded: " ++ Array:fold((+), gint(0), Array:map(int16ToInt64, arr)));
    println("AndMapped: " ++ Array:andmap(fun(x) x > gint(400), arr));

    println("Build StaticElab: " ++ Array:build(10, fun (x) x*10));
    println("Build Dynamic: " ++ Array:build(w`width - w`width + 10, fun (x) x*10));
  };

  emit ();
}
