
fun assert(b) if not(b) then wserror("Assert failed.");

foo1 :: Int -> Int = foreign("foo", ["./foo.c"])
foo2 :: Int -> Int = foreign("foo", ["./foo.c"])

//bar = fun(n) n + 100
//bar :: Int -> Int = foreign "bar" in ["bar.h", "bar.so"]

// I thought I could get away without the header for .o files.  
// And you can in C, but not in C++...
bar :: Int -> Int = foreign("bar", ["bar.h", "./bar.o"])
//bar :: Int -> Int = foreign "bar" in ["bar.h", "bar.a"]


//box = (foreign_box    "bar_box" in "foo.c" :: Stream Int -> Stream Int);
//src = (foreign_source "bar_src" in "foo.c" :: Stream Int);


main = iterate _ in timer(30.0) { 
  state {
    _ = SHELL("gcc -c bar.c")
  }
  x = bar $ foo2 $ foo1 $ 3;
  assert(x == 109);
  emit x;
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
