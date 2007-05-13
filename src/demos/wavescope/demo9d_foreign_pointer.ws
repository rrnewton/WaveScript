
fun assert(b) if not(b) then wserror("Assert failed.");

malloc :: Int -> Pointer "void*" = foreign("malloc", ["libc.so.6"], [])
free   :: Pointer "void*" -> () = foreign("free",   ["libc.so.6"], [])

BASE <- iterate _ in timer(30.0) { 
  p1 = malloc(300);
  print("  malloc'd pointer: "++ p1 ++"\n");
  p2 = malloc(300);
  print("  malloc'd pointer: "++ p2 ++"\n");
  free(p1);
  free(p2);
  print("  successfully freed\n");
  emit (p1,p2);
}

/*
send :: (Foo, Int, Bar) -> Baz = foreign "Csendfun" in "NetFuns.c"
fun network_sink (strm) {
  iterate x in strm {
    state{ counter = 0 }
    send(x, counter, other_options);
    count += 1;
  }
}
*/
