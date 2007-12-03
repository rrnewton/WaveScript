
fun assert(b) if not(b) then wserror("Assert failed.");

//plat = GETENV("OSTYPE")
plat = SHELL("uname")
libc = if plat == "Linux\n" 
       then "libc.so.6" 
       else if plat == "Darwin\n"
       then "libc.dylib" 
       else wserror("Don't know how to find libc on platform: "++ plat)

malloc :: Int -> Pointer "void*" = foreign("malloc", [libc])
free   :: Pointer "void*" -> ()  = foreign("free",   [libc])

main = iterate _ in timer(30.0) { 
  print("Running on platform: "++plat++"\n");
 
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
